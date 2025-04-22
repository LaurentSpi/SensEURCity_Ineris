rm(list = ls())

# Set directory
setwd("C:/Users/diallo/Documents/Ineris1")


# Charger les bibliothèques nécessaires
library(dplyr)
library(data.table)
library(openair)


# Définition du répertoire contenant les fichiers CSV
directory <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/dataset"


# Importation de fichiers de données 
file_list <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
file_list <- file_list[!grepl("ANT_REF_R801_Fidas_UTC", file_list)]

# Spécification des dates de début et de fin à considérer
start_date <- as.POSIXct("2020-06-18 01:00:00" , tz="UTC")
end_date <- as.POSIXct("2021-02-16 01:00:00" , tz="UTC")



# Liste pour stocker les fichiers de données modifiés
data_list <- list()

# Boucle pour lire chaque fichier CSV de capteur, effectuer les modifications et stocker dans la liste
for (file in file_list) {
  
  # Lire le fichier CSV
  data <- fread(file)
  
  # Suppression des lignes en dehors de l'intervalle de temps d'intérêt
  data[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  data <- data[data$date >= start_date & data$date <= end_date, ]
  
  # Agrégation horaire des données
  data_hourly <- timeAverage(data, avg.time = "hour", statistic = "mean", type = "Location.ID")
  
  # Ajouter le data frame modifié à la liste
  data_list[[basename(file)]] <- data_hourly
}

View(data_list)

# On a constaté que les data frames n'avaient pas le meme nombre de colonnes,
# donc on va essayer de régler ce probleme avant de passer

# Fusionner les data frames
data_all <- bind_rows(data_list)

View(data_all)



##########################
##########################
###                    ###
###        OPC         ###
###                    ###
##########################
##########################



# colonne à garder 
columns_to_keep_OPCN3 <- c("date", "latitude", "longitude", "Location.ID", "OPCN3PM10" , "OPCN3PM25")


# Initialiser une liste pour stocker les data frames OPC
data_list_OPC <- list()

# Boucle pour lire chaque fichier CSV de capteur, effectuer les modifications et stocker dans la liste
for (file in file_list) {
  
  
  # Lire le fichier CSV
  data_OPC <- fread(file)
  
  # Suppression des lignes en dehors de l'intervalle de temps d'intérêt
  data_OPC[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  data_OPC <- data_OPC[data_OPC$date >= start_date & data_OPC$date <= end_date, ]
  
  
  # Suppression des colonnes non désirées (ajustez en fonction des colonnes qu'on veut garder)
  data_OPC <- data_OPC[, ..columns_to_keep_OPCN3]
  
  
  # Agrégation horaire des données
  data_hourly_OPC <- timeAverage(data_OPC, avg.time = "hour", statistic = "mean", type = "Location.ID")
  
  
  # Ajouter le data frame modifié à la liste
  data_list_OPC[[basename(file)]] <- data_hourly_OPC
  
}

length(data_list_OPC)

# Maintenant, on a une liste de data frames modifiés pour chaque capteur

print(data_list_OPC[[1]])

# Fusionner tous les data frames en un seul
LCS_OPCN3_df_all <- do.call(rbind, data_list_OPC)


#créer la colonne ID en utilisant les rownames 
LCS_OPCN3_df_all$ID <- gsub("\\.csv.*", "", rownames(LCS_OPCN3_df_all))


# Réorganisation des colonnes
LCS_OPCN3_df_all <- LCS_OPCN3_df_all %>%
  select(ID, date, latitude, longitude, Location.ID, OPCN3PM10, OPCN3PM25)

View(LCS_OPCN3_df_all)

# Écrire le data frame fusionné dans un fichier CSV
fwrite(LCS_OPCN3_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/LCS_OPCN3_df_all.csv")



##########################
##########################
###                    ###
###        PMS         ###
###                    ###
##########################
##########################



# Initialisation d'une liste pour stocker les dataframes PMS
data_list_PMS <- list()


# colonne à garder 
columns_to_keep_PMS <- c("date", "latitude", "longitude", "Location.ID", "5310CAT" , "5325CAT")


for (file in file_list) {
  # Lire le fichier CSV
  data_PMS <- fread(file)
  
  # Suppression des lignes en dehors de l'intervalle de temps d'intérêt
  
  data_PMS[, date := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  data_PMS <- data_PMS[data_PMS$date >= start_date & data_PMS$date <= end_date, ]
  
  
  # Suppression des colonnes non désirées (ajustez en fonction des colonnes que vous voulez garder)
  # columns_to_keep <- c("date", "latitude", "longitude", "Location.ID", "datetime" , "OPCN3PM10" , "OPCN3PM25")
  data_PMS <- data_PMS[, ..columns_to_keep_PMS]
  
  
  # Agrégation horaire des données
  data_hourly_PMS <- timeAverage(data_PMS, avg.time = "hour", statistic = "mean" , type="Location.ID")
  
  
  # Ajouter le data frame modifié à la liste
  data_list_PMS[[basename(file)]] <- data_hourly_PMS
  
}


# Maintenant, nous avons une liste de data frames modifiés pour chaque capteur


print(data_list_PMS[[1]])


# Fusion de tous les data frames en un seul
LCS_PMS_df_all <- do.call(rbind, data_list_PMS)

#création de la colonne ID en utilisant les rownames 
LCS_PMS_df_all$ID <- gsub("\\.csv.*","",rownames(LCS_PMS_df_all))


# Réorganisation des colonnes
LCS_PMS_df_all <- LCS_PMS_df_all %>%
  select(ID, date, latitude, longitude, Location.ID, "5310CAT", "5325CAT")

# Renommage de colonne 
names(LCS_PMS_df_all)[names(LCS_PMS_df_all) == "5310CAT"] <- "PMS_PM10"
names(LCS_PMS_df_all)[names(LCS_PMS_df_all) == "5325CAT"] <- "PMS_PM25"

LCS_PMS_df_all[which(LCS_PMS_df_all$ID=="Antwerp_4043A7"),]$Location.ID <-"ANT_REF_R802"
LCS_PMS_df_all[which(LCS_PMS_df_all$ID=="Antwerp_40499C"),]$Location.ID <-"ANT_REF_R801"
LCS_PMS_df_all[which(LCS_PMS_df_all$ID=="Antwerp_4049A6"),]$Location.ID <-"ANT_REF_R802"
LCS_PMS_df_all[which(LCS_PMS_df_all$ID=="Antwerp_408168"),]$Location.ID <-"ANT_URB_PARK"
LCS_PMS_df_all[which(LCS_PMS_df_all$ID=="Antwerp_4043B1"),]$Location.ID <-"ANT_REF_R801"


View(LCS_PMS_df_all)

# Écrire le data frame fusionné dans un fichier CSV
fwrite(LCS_PMS_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/LCS_PMS_df_all.csv")


# Vérification du nombre de capteurs et nombre de positions
nbr_locations <- length(unique(LCS_PMS_df_all$Location.ID))
nbr_sensors <- length(unique(LCS_PMS_df_all$ID))
print(paste("Nombre de capteurs:", nbr_sensors))
print(paste("Nombre de positions:", nbr_locations))




##########################
##########################
###                    ###
###        REF         ###
###                    ###
##########################
##########################




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

View(ref_df_all)

# Afficher ou sauvegarder le dataframe final
print(ref_df_all)


rownames(ref_df_all) <- NULL


# Écrire le data frame fusionné dans un fichier CSV
fwrite(ref_df_all, "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/ref_df_all.csv")


nbr_sta <- length(unique(ref_df_all$Location.ID))

print(paste("Nombre de station de référence:", nbr_sta))

