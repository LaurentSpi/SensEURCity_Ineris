rm(list = ls())

# Set directory
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1")


# Import libraries
library(dplyr)
library(data.table)
library(chron)
library(ggplot2)
library(stats)
library(RColorBrewer)
library(stringr)
library(sf) # Chargement de la bibliothèque pour la manipulation spatiale

# Import libraries
library(raster)
library(maptools)
library(sp)
library(RColorBrewer)
library(fields)
library(rgdal)
library(tidyr)
require(sp)
require(rgdal)

# install.packages("eurostat")
library(eurostat)

# Directory paths
indir   <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/"# path for input directory
outdir  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/"# path for output directory
outdir2  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/"# path for output directory
outdir3 <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/outliers_detection TEST"


# Chemin du fichier - ajustez selon votre configuration
metadata_sites <- fread(paste0(indir, "metadata/metadata_sites.csv"), fill=TRUE)
metadata_sites <- as.data.frame(metadata_sites)

unique(metadata_sites$location_id)

# DATA <- metadata_sites
# DATA$Location.ID <- as.character(DATA$Location.ID)
# DATA$Location.ID[DATA$Location.ID == "ANT_TRA_KIPD"] <- "ANT_URB_KIPD"


# Filtrage et transformation des données
metadata_sites <- metadata_sites %>%
  filter(grepl("^ANT_", location_id)) %>%  # Filtre pour garder seulement les lignes commençant par "ANT_"
  dplyr::select(1:6) %>%  # Sélectionne uniquement les six premières colonnes
  mutate(Typology = case_when(
    location_id %in% c("ANT_REF_AL01", "ANT_REF_M802") ~ "INDUS",
    location_id %in% c("ANT_REF_R801", "ANT_REF_R803", "ANT_REF_R811", "ANT_REF_R817") ~ "URB",
    location_id %in% c("ANT_REF_R802", "ANT_REF_R804", "ANT_REF_R805") ~ "TRA",
    TRUE ~ as.character(str_extract(location_id, "(?<=ANT_)[^_]+")) # Fallback si aucun match n'est trouvé
  ))


names(metadata_sites)[names(metadata_sites) == "location_id"] <- "Location.ID"
names(metadata_sites)[names(metadata_sites) == "latitude_dd"] <- "Y"
names(metadata_sites)[names(metadata_sites) == "longitude_dd"] <- "X"

# metadata_sites <- metadata_sites %>%
# rename(Location.ID = location_id)


# Affichage des premières lignes du résultat filtré
head(metadata_sites)



# location_data <-  metadata_sites %>%
#   dplyr::select(Location.ID = Location.ID, Y = latitude_dd, X = longitude_dd)

location_data <-  metadata_sites %>%
  dplyr::select(Location.ID = Location.ID, Y = Y, X = X)



# Fonction pour convertir les coordonnées
convert_coords <- function(df, lon, lat) {
  
  # Assurer que df est un data.frame pour éviter des problèmes avec data.table
  df <- as.data.frame(df)
  
  
  # Création objet sf 
  df_sf <- st_as_sf(df, coords = c(lon, lat), crs = 4326)
  # Transformation en coordonnées UTM (zone 31N pour Antwerp, Belgique)
  df_sf_meters <- st_transform(df_sf, crs = 32631)
  # Extraction des coordonnées transformées
  coords <- st_coordinates(df_sf_meters)
  # Remplacement des colonnes originales par les nouvelles coordonnées transformées
  df[lon] <- coords[, 1] 
  df[lat] <- coords[, 2]  
  return(df)
}

# Application de la fonction de conversion sur ref_df_all et LCS_PMS_df_all
location_data <- convert_coords(location_data, "X", "Y")


#Define projections
CRS_L93=CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
CRS_BELG=CRS("+init=epsg:32631")

# # Define projections
# CRS_L93 <- st_crs(2154)
# CRS_BELG <- st_crs(28992)

library(raster)

legend_CLC <- read.csv(file=paste0(indir,"SIG_data/legend_CLC_V4.csv"), header=TRUE, sep=";",skip=0)

CLC_Netherlands_NT <- raster(paste0(indir,"SIG_data/CLC_Netherlands_NT.tif"))
crs_raster <- crs(CLC_Netherlands_NT)
crs_raster

## 1) CLC
# Reprojection vers UTM (choisis la bonne zone UTM pour les Pays-Bas)
extent(CLC_Netherlands_NT)
Xmin=-149435.2  ; Xmax=412450.6   ; Ymin=-17499.31    ;Ymax=720698.7     # define x and y max/min based on coast lines

library(terra)

# Charger le raster avec terra
CLC_Netherlands_NT_terra <- rast(CLC_Netherlands_NT)

# Cropping avant reprojection
e <- ext(Xmin, Xmax, Ymin, Ymax)
CLC_Netherlands_NT_crop <- crop(CLC_Netherlands_NT_terra, e)

# Reprojection
crs_utm <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
CLC_Netherlands_NT_utm <- project(CLC_Netherlands_NT_terra, y = crs_utm)



# Convert df to spatial object
splocation_data=location_data # crop sensor data
# splocation_data$Long <- splocation_data$X; splocation_data$Lat <- splocation_data$Y
coordinates(splocation_data)=~X+Y
proj4string(splocation_data)=CRS_BELG
tmp = splocation_data@coords; dlon=tmp[,1]; dlat=tmp[,2]
splocation_data$lon <- dlon; splocation_data$lat <- dlat
class(splocation_data); summary(splocation_data)


# Extract raster value at sensor location based on a buffer of 1m
CLC_sens <- raster::extract(CLC_Netherlands_NT,             # raster layer
                            splocation_data,   # SPDF with centroids for buffer
                            buffer = 1,    # buffer size, units depend on CRS
                            fun=median,  # what to value to extract
                            na.rrm=TRUE,   # rm NAs
                            df=TRUE)       # return a dataframe? 


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

# Créer le tableau de comparaison
Typo_data_comparison <- data.frame(
  Typo_sEURcity = merged_data$Typology.x,  # Typology de metadata_sites
  Typo_CLC = merged_data$Typology.y       # Typology de location_data
)


# Assurez-vous que les deux colonnes sont correctement nommées et de type caractère
metadata_sites$Location.ID <- as.character(metadata_sites$Location.ID)
location_data$Location.ID <- as.character(location_data$Location.ID)

# Trouver les lignes dans metadata_sites qui n'ont pas de correspondants dans location_data
missing_entries <- anti_join(metadata_sites, location_data, by = "Location.ID")
print(missing_entries)


# Préparation des données pour la fusion
metadata_typo <- metadata_sites %>%
  dplyr::select(Location.ID, Typology)

location_typo <- location_data %>%
  dplyr::select(Location.ID, Typology)

# Fusionner les données en conservant toutes les entrées de metadata_sites
typo_comparison <- left_join(metadata_typo, location_typo, by = "Location.ID", suffix = c(".sEURcity", ".CLC"))

# Remplir les valeurs manquantes pour Typology.CLC avec les valeurs de Typology.sEURcity
typo_comparison$Typology.CLC <- ifelse(is.na(typo_comparison$Typology.CLC), typo_comparison$Typology.sEURcity, typo_comparison$Typology.CLC)

typo_comparison[typo_comparison$Location.ID == "ANT_REF_R802", "Typology.CLC"] <- "Traffic"

dir.create("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/OUTPUTS1/", recursive = TRUE)


fwrite(typo_comparison, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/OUTPUTS1/PMS_typo_CLC_BDD_comparison.csv")

save(typo_comparison,file=paste0(outdir,"PMS_V2_typo_CLC_BDD_comparison.Rda"))
















