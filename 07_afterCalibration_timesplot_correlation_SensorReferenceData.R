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
load(file_calibratedSensorsAlltime_Rda) # A modifier en fonction du fichier test
load(file_ref_df_all_rda)
load(file_typology_sens_Rda)

LocID <- typology_sens
LocID <- LocID[ , -3]

head(ref_df_all)
head(calibratedSensorsAlltime3)
calibratedSensorsAlltime <- calibratedSensorsAlltime3

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
  
  # --- Sécurité : s'assurer que la colonne 'date' est bien POSIXct
  if (!"date" %in% names(station_data)) stop("Colonne 'date' absente de station_data")
  if (!inherits(station_data$date, "POSIXct")) {
    station_data$date <- as.POSIXct(station_data$date, tz = "UTC")
  }
  
  # --- Masquage ciblé : période "gelée" pour 4043AE uniquement (les autres séries restent inchangées)
  freeze_start <- as.POSIXct("2020-08-27 00:00:00", tz = "UTC")
  freeze_end   <- as.POSIXct("2020-10-01 00:00:00", tz = "UTC")
  mask_freeze  <- station_data$date >= freeze_start & station_data$date <= freeze_end
  
  # Détecter de façon robuste la/les colonne(s) PM2.5 de 4043AE (ex. "PM2.5_4043AE" ou "PM2.5_PMS5003_4043AE")
  cols_4043AE <- grep("^PM2\\.5(?:_[A-Za-z0-9]+)?_4043AE$", names(station_data), value = TRUE)
  if (length(cols_4043AE) == 0L && "PM2.5_4043AE" %in% names(station_data)) cols_4043AE <- "PM2.5_4043AE"
  if (length(cols_4043AE) > 0L) {
    for (cn in cols_4043AE) {
      station_data[[cn]][mask_freeze] <- NA
    }
  }
  
  # -------------------------------
  # 1) Graphiques de corrélation
  # -------------------------------
  for (sensor_id in sensor_ids) {
    y_col <- paste0("PM2.5_", sensor_id)
    if (!("PM2.5" %in% names(station_data)) || !(y_col %in% names(station_data))) {
      # Colonne manquante : on passe ce capteur
      next
    }
    
    png(
      filename = file.path(path_corr, paste0("Correlation ", station_id_short, "-", sensor_id, ".png")),
      units = "cm", res = 300, width = WidthEtalonnage, height = HeightEtalonnage
    )
    
    op <- par(no.readonly = TRUE)
    par(mar = c(5, 5.5, 1.5, 1.5), oma = c(0, 0, 0, 0), xaxs = "r", yaxs = "r")
    
    # Tracé principal
    Limit.XY <- Etalonnage(
      x = station_data[, "PM2.5"],
      s_x = NULL,
      y = station_data[, y_col],
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
    
    # Ligne y = x + légende compacte
    abline(a = 0, b = 1, col = "green4", lwd = 1.2)
    legend("topleft", legend = "Ligne y = x", lty = 1, col = "green4", bty = "n", cex = 0.9)
    
    # Régression/annotations
    Cal_Line(
      x = station_data[, "PM2.5"],
      s_x = NULL,
      y = station_data[, y_col],
      s_y = NULL,
      Mod_type = "Linear",
      Matrice = NULL,
      line_position = -1.1,   # un peu remonté pour éviter le bord
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
    
    par(op)
    dev.off()
  }
  
  # -------------------------------
  # 2) Séries temporelles
  # -------------------------------
  # Colonnes à tracer (référence + capteurs de la station)
  ts_cols <- c("PM2.5", paste0("PM2.5_", sensor_ids))
  ts_cols <- intersect(ts_cols, names(station_data))  # on garde seulement celles présentes
  
  if (length(ts_cols) >= 2) {
    n_series <- length(ts_cols)
    if (length(needed_colors) < n_series) {
      needed_colors <- rep(needed_colors, length.out = n_series)
    }
    
    # Libellés lisibles
    name_pol <- ts_cols
    name_pol[ts_cols == "PM2.5"] <- paste0("PM2.5_FIDAS200_", station_id_short)
    name_pol[grepl("^PM2\\.5_", ts_cols)] <- paste0(
      "PM2.5_PMS5003_",
      sub("^PM2\\.5_(?:[A-Za-z0-9]+_)?", "", ts_cols) # conserve l'ID final
    )
    
    png(
      filename = file.path(path_timeseries, paste0("Time series ", station_id_short, " ", paste(sensor_ids, collapse = "-"), ".png")),
      units = "cm", res = 300, width = WidthTimeplot, height = HeightTimeplot
    )
    
    op <- par(no.readonly = TRUE)
    par(mar = c(4.5, 4.5, 1.2, 1.2), oma = c(0, 0, 0, 0))
    
    timePlot(
      mydata      = station_data,
      pollutant   = ts_cols,
      plot.type   = "l",
      lwd         = 1.5,
      group       = FALSE,
      main        = "",
      ylab        = "",
      name.pol    = name_pol,
      auto.text   = FALSE,
      date.format = "%d/%m",
      cols        = needed_colors[seq_len(n_series)],
      key         = TRUE,
      key.columns = 2,
      key.position= "top",
      y.relation  = "free"
    )
    
    par(op)
    dev.off()
  }
}


# Parcourir chaque station et générer les graphiques
for (station_id in names(stations_sensors)) {
  sensor_ids <- stations_sensors[[station_id]]
  station_data_id <- paste0("REF_", gsub("ANT_REF_", "", station_id), "_data")
  station_data <- get(station_data_id)
  plots_generator(station_data, station_data_id, sensor_ids, path_correlation_plots, path_timeseries_plots) # Modifier en fonction du path test
}


