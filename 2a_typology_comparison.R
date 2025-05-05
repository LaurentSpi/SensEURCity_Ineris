rm(list = ls())

# Charger le fichier de configuration global
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

# Import libraries
library(dplyr)
library(data.table)
library(chron)
library(ggplot2)
library(stats)
library(RColorBrewer)
library(stringr)
library(sf)
library(sp)
library(raster)
library(RColorBrewer)
library(spam)
library(fields)
library(tidyr)
library(eurostat)

# Chargement des métadonnées des sites
metadata_sites <- fread(file_metadata_sites, fill=TRUE)
metadata_sites <- as.data.frame(metadata_sites)

unique(metadata_sites$location_id)

# DATA <- metadata_sites
# DATA$Location.ID <- as.character(DATA$Location.ID)
# DATA$Location.ID[DATA$Location.ID == "ANT_TRA_KIPD"] <- "ANT_URB_KIPD"


# Filtrage et transformation des données
metadata_sites <- metadata_sites %>%
  filter(grepl("^ANT_", location_id)) %>%  # Filtre pour garder seulement les lignes commençant par "ANT_"
  dplyr::select(1:6) %>%
  mutate(Typology = case_when(
    location_id %in% c("ANT_REF_AL01", "ANT_REF_M802") ~ "INDUS",
    location_id %in% c("ANT_REF_R801", "ANT_REF_R803", "ANT_REF_R811", "ANT_REF_R817") ~ "URB",
    location_id %in% c("ANT_REF_R802", "ANT_REF_R804", "ANT_REF_R805") ~ "TRA",
    TRUE ~ as.character(str_extract(location_id, "(?<=ANT_)[^_]+")) # Fallback si aucun match n'est trouvé
  ))


names(metadata_sites)[names(metadata_sites) == "location_id"] <- "Location.ID"
names(metadata_sites)[names(metadata_sites) == "latitude_dd"] <- "Y"
names(metadata_sites)[names(metadata_sites) == "longitude_dd"] <- "X"

head(metadata_sites)

location_data <-  metadata_sites %>%
  dplyr::select(Location.ID = Location.ID, Y = Y, X = X)


# Fonction pour convertir les coordonnées
convert_coords <- function(df, lon, lat) {
  df <- as.data.frame(df)
  df_sf <- st_as_sf(df, coords = c(lon, lat), crs = 4326)
  df_sf_meters <- st_transform(df_sf, crs = 32631)
  coords <- st_coordinates(df_sf_meters)
  df[lon] <- coords[, 1] 
  df[lat] <- coords[, 2]  
  return(df)}

location_data <- convert_coords(location_data, "X", "Y")


library(terra)
legend_CLC <- read.csv(file=file_CLC_legend_csv, header=TRUE, sep=";",skip=0)
CLC <- rast(file_CLC_raster_tif)
print(crs(CLC))

e <- ext(-149435.2, 412450.6, -17499.31, 720698.7)

CLC_utm <- CLC |>
  crop(e) |>
  project("EPSG:32631", method = "near")
plot(CLC_utm, main = "CLC cropped & reprojeté UTM31N")

loc_vect <- vect(location_data, geom = c("X","Y"), crs = "EPSG:32631")
CLC_sens <- extract(CLC_utm, loc_vect, buffer = 1, fun = median)
head(CLC_sens)


CLC_sens$CLC_Netherlands_NT[CLC_sens$CLC_Netherlands_NT==128]<-NA

location_data$CLC <- CLC_sens$CLC_Netherlands_NT
CLC_val <- legend_CLC
CLC_val=subset(CLC_val,select=-c(v2,v3,v4,v5,v6))
colnames(CLC_val)[1] <- "CLC"
names(CLC_val)[names(CLC_val) == "type"] <- "LandType"
names(CLC_val)[names(CLC_val) == "type2"] <- "Typology"

location_data=merge(location_data,CLC_val,by="CLC")
unique(metadata_sites$Location.ID)
unique(location_data$Location.ID)

missing_entries <- anti_join(metadata_sites, location_data, by = "Location.ID")
location_data <- bind_rows(location_data, missing_entries)

# Fusionner les données sur l'ID commun
merged_data <- merge(metadata_sites, location_data, by = "Location.ID", all = FALSE)

Typo_data_comparison <- data.frame(
  Typo_sEURcity = merged_data$Typology.x,  # Typology de metadata_sites
  Typo_CLC = merged_data$Typology.y       # Typology de location_data
)


metadata_sites$Location.ID <- as.character(metadata_sites$Location.ID)
location_data$Location.ID <- as.character(location_data$Location.ID)

missing_entries <- anti_join(metadata_sites, location_data, by = "Location.ID")
print(missing_entries)


# Préparation des données pour la fusion
metadata_typo <- metadata_sites %>%
  dplyr::select(Location.ID, Typology)

location_typo <- location_data %>%
  dplyr::select(Location.ID, Typology)


typo_comparison <- left_join(metadata_typo, location_typo, by = "Location.ID", suffix = c(".sEURcity", ".CLC"))
typo_comparison$Typology.CLC <- ifelse(is.na(typo_comparison$Typology.CLC), typo_comparison$Typology.sEURcity, typo_comparison$Typology.CLC)
typo_comparison[typo_comparison$Location.ID == "ANT_REF_R802", "Typology.CLC"] <- "Traffic"

write.csv(typo_comparison, file_typo_CLC_BDD_comparison_csv)
save(typo_comparison, file=file_typo_CLC_BDD_comparison_Rda)
