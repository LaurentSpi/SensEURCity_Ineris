rm(list = ls())

# Charger le fichier de configuration global
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

## List of packages to install
Packages <- c("openair")
do.call("library", as.list("openair"))

# Import libraries
library(dplyr)
library(data.table)
library(chron)
library(ggplot2)
library(stats)
library(stringr)
library(sf)
library(sp)
library(RColorBrewer)
library(fields)
library(tidyr)

# Load .Rda
load(file_calibratedSensorsAlltime_Rda)
load(file_ref_df_all_rda)
load(file_typology_sens_Rda)

LocID <- typology_sens
LocID <- LocID[ , -3]

head(ref_df_all)
head(calibratedSensorsAlltime)

#############

# > print(stations)
# [1] "ANT_REF_R817" ""             "ANT_REF_R801" "ANT_REF_R804" "ANT_REF_R802" "ANT_REF_R811"
# [7] "ANT_REF_AL01" "ANT_REF_M802" "ANT_REF_R805" "ANT_REF_R803"
# > ref_df_all[which(ref_df_all$ID==""),]
# ID            datetime PM10 PM2.5      X       Y Representativity_min
# 5825    2020-06-18 05:00:00 12.6     8 600013 5674119                  100


##############
ref_df_all <- ref_df_all %>% filter(ID != "")
stations <- unique(ref_df_all$ID) 
print(stations)

subset(calibratedSensorsAlltime, PM2.5 == 0)
colocated_calibratedSensorsAlltime <- calibratedSensorsAlltime

# Liste des valeurs à garder dans la colonne ID
values_to_keep <- c("Antwerp_40499C", "Antwerp_4043B1", "Antwerp_4049A6", 
                    "Antwerp_4043A7", "Antwerp_40499F", "Antwerp_4043AE", 
                    "Antwerp_4067B3", "Antwerp_40642B", "Antwerp_4047D7", 
                    "Antwerp_4065EA", "Antwerp_4067BD", "Antwerp_4065DA")

# Filtrer les lignes en fonction des valeurs de la colonne ID
colocated_calibratedSensorsAlltime <- colocated_calibratedSensorsAlltime %>%
  filter(ID %in% values_to_keep)

colocated_calibratedSensorsAlltime <- merge(colocated_calibratedSensorsAlltime, LocID, by = "ID", all.x = TRUE)

save(colocated_calibratedSensorsAlltime, file=file_colocated_calibratedSensorsAlltime_Rda)

ref_df_all <- ref_df_all %>% rename(date = datetime)

# dataframe pour une station de référence
create_station_df <- function(station_id, sensor_ids, ref_df, cap_df) {
  station_ref_df <- ref_df %>% filter(ID == station_id)
  for (sensor_id in sensor_ids) {
    sensor_data <- cap_df %>% filter(ID == paste0("Antwerp_", sensor_id)) %>% 
      dplyr::select(datetime, CalibratedPM2.5) %>% rename(!!paste0("PM2.5_", sensor_id) := CalibratedPM2.5)
    station_ref_df <- inner_join(station_ref_df, sensor_data, by = c("date" = "datetime"),
                                 relationship = "many-to-many")
  }
  
  return(station_ref_df)
}

# liste de dataframes pour chaque station de référence
station_dfs <- lapply(names(stations_sensors), function(station_id) {
  create_station_df(station_id, stations_sensors[[station_id]], ref_df_all, colocated_calibratedSensorsAlltime)
})

names(station_dfs) <- lapply(names(stations_sensors), function(station_id) {
  paste0("REF_", gsub("ANT_REF_", "", station_id), "_data")
})

head(station_dfs[[1]])

# Créer le dossier s'il n'existe pas (normalement déjà créé par le fichier de configuration)
if (!dir.exists(path_correlation_after_calibration)) {
  dir.create(path_correlation_after_calibration, recursive = TRUE)
}

REF_R801_data <- station_dfs[[1]]
REF_R802_data <- station_dfs[[2]]
REF_R804_data <- station_dfs[[3]]
REF_R805_data <- station_dfs[[4]]
REF_R811_data <- station_dfs[[5]]
REF_R817_data <- station_dfs[[6]]
REF_M802_data <- station_dfs[[7]]
REF_R803_data <- station_dfs[[8]]
REF_AL01_data <- station_dfs[[9]] 

# Sauvegarder les dataframes dans les chemins définis dans le fichier de configuration
save(REF_R801_data, file = files_ref_sensor_data_list$REF_R801_data)
save(REF_R802_data, file = files_ref_sensor_data_list$REF_R802_data)
save(REF_R804_data, file = files_ref_sensor_data_list$REF_R804_data)
save(REF_R805_data, file = files_ref_sensor_data_list$REF_R805_data)
save(REF_R811_data, file = files_ref_sensor_data_list$REF_R811_data)
save(REF_R817_data, file = files_ref_sensor_data_list$REF_R817_data)
save(REF_M802_data, file = files_ref_sensor_data_list$REF_M802_data)
save(REF_R803_data, file = files_ref_sensor_data_list$REF_R803_data)
save(REF_AL01_data, file = files_ref_sensor_data_list$REF_AL01_data)

## two R files with functions to source the SensorIneris_Toolbox.R, usually in the OneDrive - INERIS/SensorIneris/RScript folder
source(choose.files(caption = "Select SensorIneris_Toolbox.R file"))
source(choose.files(caption = "Select uBss and uCi.R file"))

## replace NaN with NA in the subset database
MyDataFrame <- c()
MyDataFrame[is.nan.data.frame(MyDataFrame)] <- NA

# Utilisation des paramètres de taille d'image définis dans le fichier de configuration
WidthTimeplot <- figure_sizes$WidthTimeplot
HeightTimeplot <- figure_sizes$HeightTimeplot
WidthEtalonnage <- figure_sizes$WidthEtalonnage
HeightEtalonnage <- figure_sizes$HeightEtalonnage

plots_generator <- function(station_data, station_id, sensor_ids, path_corr, path_timeseries) {
  station_id_short <- gsub("_data$", "", station_id)
  colors_warehouse <- c("red","black","blue","purple3","green4","gold","pink4")
  needed_colors <- colors_warehouse[1:(length(sensor_ids) + 1)]
  
  # graphique de séries temporelles
  timePlot(
    mydata = station_data,
    pollutant = c("PM2.5", paste0("PM2.5_", sensor_ids)), 
    plot.type = "l",
    lwd = 1.5,
    group = FALSE,
    main = "",
    ylab = "",
    name.pol = c(paste0("PM2.5_FIDAS200_", station_id_short), paste0("PM2.5_PMS5003_", sensor_ids)), 
    auto.text = FALSE,
    date.format = "%d/%m",
    cols = needed_colors, 
    key = TRUE,
    key.columns = 2,
    key.position = "top",
    y.relation = "free"
  )
  
  dev.copy(png, filename = file.path(path_timeseries, paste0("Time series ", station_id_short, " ", paste(sensor_ids, collapse = "-"), ".png")),
           units = "cm", res = 1024, width = WidthTimeplot, height = HeightTimeplot)
  dev.off()
  
  # graphiques de corrélation pour chaque capteur
  for (sensor_id in sensor_ids) {
    Limit.XY <- Etalonnage(
      x = station_data[, "PM2.5"],
      s_x = NULL,
      y = station_data[, paste0("PM2.5_", sensor_id)],
      s_y = NULL,
      AxisLabelX = paste0("PM2.5_FIDAS200_", station_id_short),
      AxisLabelY = paste0("PM2.5_PMS5003_", sensor_id),
      Title = "",
      Marker = 19,
      Couleur = "blue",
      ligne = "p",
      XY_same = FALSE,
      lim = NULL,
      steps = c(10, 10),
      digitround = NULL,
      marges = NULL,
      PlotAxis = "s",
      OrdonneeOrigine = NULL
    )
    
    lines(x= c(min(Limit.XY),max(Limit.XY)), y=c(min(Limit.XY),max(Limit.XY)), type = "l", col = "green4")
    mtext(paste0("Line Y=X "), line=-36.3, adj=1, padj=0, col= "green4", cex=1.2)
    
    Cal_Line(
      x = station_data[, "PM2.5"],
      s_x = NULL,
      y = station_data[, paste0("PM2.5_", sensor_id)],
      s_y = NULL,
      Mod_type = "Linear",
      Matrice = NULL,
      line_position = -1.3,
      Couleur = "red",
      Sensor_name = NULL,
      f_coef1 = "%.2f",
      f_coef2 = "%.2f",
      f_R2 = "%.3f",
      lim = Limit.XY,
      marges = NULL,
      Covariates = NULL,
      Equation = "RMSE"
    )
    
    dev.copy(png, filename = file.path(path_corr, paste0("Correlation ", station_id_short, "-", sensor_id, ".png")),
             units = "cm", res = 1024, width = WidthEtalonnage, height = HeightEtalonnage)
    dev.off()
  }
}

# Parcourir chaque station et générer les graphiques
for (station_id in names(stations_sensors)) {
  sensor_ids <- stations_sensors[[station_id]]
  station_data_id <- paste0("REF_", gsub("ANT_REF_", "", station_id), "_data")
  station_data <- get(station_data_id)
  plots_generator(station_data, station_data_id, sensor_ids, path_correlation_plots, path_timeseries_plots)
}


