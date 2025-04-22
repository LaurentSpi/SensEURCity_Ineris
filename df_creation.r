rm(list = ls())

# Set directory
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1")

# Charger les bibliothèques nécessaires
library(data.table)
library(openair)
library(tidyverse)

# Définition du répertoire contenant les fichiers CSV
directory <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/dataset" # Pour les capteurs PMS

# Importation de fichiers de données 
file_list <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
file_list <- file_list[!grepl("ANT_REF_R801_Fidas_UTC", file_list)]


start_date <- as.POSIXct("2020-06-18 01:00:00" , tz="UTC")
end_date <- as.POSIXct("2021-02-16 01:00:00" , tz="UTC")



#                                      ##########################
#                                      ##########################
#                                      ###                    ###
#                                      ###        OPC         ###
#                                      ###                    ###
#                                      ##########################
#                                      ##########################
# 
# 
# 
# 
# 
# # Initialiser une liste pour stocker les data frames OPC
# data_list <- list()
# 
# 
# # colonne à garder
# columns_to_keep_OPCN3 <- c("date", "latitude", "longitude", "Location.ID", "OPCN3PM10" , "OPCN3PM25")
# 
# 
# # Boucle pour lire chaque fichier CSV de capteur, effectuer les modifications et stocker dans la liste
# for (file in file_list) {
# 
# 
#   # Lire le fichier CSV
#   data <- fread(file)
# 
# 
#   # Suppression des lignes en dehors de l'intervalle de temps d'intérêt
#   data[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
#   data <- data[data$date >= start_date & data$date <= end_date, ]
# 
# 
#   # Suppression des colonnes non désirées (ajustez en fonction des colonnes qu'on veut garder)
#   data <- data[, ..columns_to_keep_OPCN3]
# 
# 
#   # Agrégation horaire des données
#   data_hourly <- timeAverage(data, avg.time = "hour", statistic = "mean", type = "Location.ID")
# 
# 
#   # Ajouter le data frame modifié à la liste
#   data_list[[basename(file)]] <- data_hourly
# 
# }
# 
# 
# print(data_list[[1]])
# 
# # Fusionner tous les data frames en un seul
# LCS_df_all <- do.call(rbind, data_list)
# 
# #créer la colonne ID en utilisant les rownames
# LCS_df_all$ID <- gsub("\\.csv.*", "", rownames(LCS_df_all))
# 
# 
# LCS_df_all <- LCS_df_all %>% filter(Location.ID != "")
# LCS_df_all$Location.ID <- as.character(LCS_df_all$Location.ID)
# LCS_df_all$Location.ID[LCS_df_all$Location.ID == "ANT_TRA_KIPD"] <- "ANT_URB_KIPD"
# 
# unique(LCS_df_all$Location.ID)
# 
# # Réorganisation des colonnes
# #install.packages("dplyr")
# library(dplyr)
# 
# LCS_df_all <- LCS_df_all %>%
#   select(ID, date, latitude, longitude, Location.ID, OPCN3PM10, OPCN3PM25)
# 
# 
# # Écrire le data frame fusionné dans un fichier CSV
# fwrite(LCS_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/INPUTS/LCS_df_all.csv")

# 
#                                           
#                                           ##########################
#                                           ##########################
#                                           ###                    ###
#                                           ###        PMS         ###
#                                           ###                    ###
#                                           ##########################
#                                           ##########################
# 
# 
# 
# Initialisation d'une liste pour stocker les dataframes PMS
data_list_PMS <- list()


# colonne à garder
columns_to_keep_PMS <- c("date", "latitude", "longitude", "Location.ID", "5310CAT" , "5325CAT")


for (file in file_list) {
  data_PMS <- fread(file)


  data_PMS[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  data_PMS <- data_PMS[data_PMS$date >= start_date & data_PMS$date <= end_date, ]


  data_PMS <- data_PMS[, ..columns_to_keep_PMS]


  # Agrégation horaire des données
  data_hourly_PMS <- timeAverage(data_PMS, avg.time = "hour", statistic = "mean" , type="Location.ID")


  # Ajouter le data frame modifié à la liste
  data_list_PMS[[basename(file)]] <- data_hourly_PMS

}



print(data_list_PMS[[1]])

LCS_df_all <- do.call(rbind, data_list_PMS)


LCS_df_all$ID <- gsub("\\.csv.*","",rownames(LCS_df_all))

LCS_df_all <- LCS_df_all %>% filter(Location.ID != "")
LCS_df_all$Location.ID <- as.character(LCS_df_all$Location.ID)
LCS_df_all$Location.ID[LCS_df_all$Location.ID == "ANT_TRA_KIPD"] <- "ANT_URB_KIPD"


LCS_df_all <- LCS_df_all %>%
  select(ID, date, latitude, longitude, Location.ID, "5310CAT", "5325CAT")

names(LCS_df_all)[names(LCS_df_all) == "5310CAT"] <- "PMS_PM10"
names(LCS_df_all)[names(LCS_df_all) == "5325CAT"] <- "PMS_PM25"

LCS_df_all[which(LCS_df_all$ID=="Antwerp_4043A7"),]$Location.ID <-"ANT_REF_R802"
LCS_df_all[which(LCS_df_all$ID=="Antwerp_40499C"),]$Location.ID <-"ANT_REF_R801"
LCS_df_all[which(LCS_df_all$ID=="Antwerp_4049A6"),]$Location.ID <-"ANT_REF_R802"
LCS_df_all[which(LCS_df_all$ID=="Antwerp_408168"),]$Location.ID <-"ANT_URB_PARK"
LCS_df_all[which(LCS_df_all$ID=="Antwerp_4043B1"),]$Location.ID <-"ANT_REF_R801"


# Écrire le data frame fusionné dans un fichier CSV
fwrite(LCS_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/INPUTS/LCS_df_all.csv")

# # Vérification du nombre de capteurs et nombre de positions
# nbr_locations <- length(unique(LCS_df_all$Location.ID))
# nbr_sensors <- length(unique(LCS_df_all$ID))
# print(paste("Nombre de capteurs:", nbr_sensors))
# print(paste("Nombre de positions:", nbr_locations))
# 
# 
# 
# 
#                                         ##########################
#                                         ##########################
#                                         ###                    ###
#                                         ###        REF         ###
#                                         ###                    ###
#                                         ##########################
#                                         ##########################
# 



# Initialiser une liste pour stocker les data frames des stations de référence
data_list_ref <- list()

# Définir les colonnes à conserver pour les fichiers de référence
columns_to_keep_ref <- c("date", "Ref.Lat", "Ref.Long", "Location.ID", "Ref.PM10", "Ref.PM2.5")

# Utiliser uniquement les fichiers des capteurs qui étaient colocalisés avec les stations de référence
colocated_sensors <- c("40499C", "4049A6", "4065EA", "4067BD", "40499F", "4067B3", "40642B", "4047D7", "4065DA") 

# Filtrer la liste des fichiers pour ne garder que ceux des capteurs colocalisés
colocated_files <- file_list[grepl(paste(colocated_sensors, collapse = "|"), file_list)]

# Boucle pour lire chaque fichier CSV des stations de référence, effectuer les modifications et stocker dans la liste
for (file in colocated_files) {
  
  # Lire le fichier CSV
  data_ref <- fread(file)
  
  # Assurer que les données sont dans l'intervalle de temps d'intérêt
  data_ref[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  data_ref <- data_ref[data_ref$date >= start_date & data_ref$date <= end_date, ]
  
  # Suppression des colonnes non désirées
  data_ref <- data_ref[, ..columns_to_keep_ref]
  
  # Agrégation horaire des données, si nécessaire
  data_hourly_ref <- timeAverage(data_ref, avg.time = "hour", statistic = "mean", type = "Location.ID")
  
  # Ajouter le data frame modifié à la liste
  data_list_ref[[basename(file)]] <- data_hourly_ref
}


print(data_list_ref[[1]])

ref_df_all <- do.call(rbind, data_list_ref)
unique(ref_df_all$Location.ID)

ref_df_all <- ref_df_all %>% filter(Location.ID != "")

print(ref_df_all)

rownames(ref_df_all) <- NULL


# Écrire le data frame fusionné dans un fichier CSV
fwrite(ref_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/INPUTS/ref_df_all.csv")

nbr_sta <- length(unique(ref_df_all$Location.ID))

print(paste("Nombre de station de référence:", nbr_sta))

