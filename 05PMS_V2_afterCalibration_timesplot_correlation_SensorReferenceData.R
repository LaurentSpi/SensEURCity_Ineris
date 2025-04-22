rm(list = ls())


## List of packages to install
Packages <- c("openair")
do.call("library", as.list("openair"))


# Set directory
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1")


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

# Directory paths
indir   <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/"# path for input directory
outdir  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/"# path for output directory
outdir2  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/"# path for output directory
outdir3 <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/outliers_detection"


# Load .Rda
load(paste0(outdir,"uBss_PMS_calibratedSensorsAlltime.Rda"))
load(paste0(indir,"ref_df_all.Rda"))
load(paste0(outdir,"PMS_typology_sens.Rda"))

LocID <- typology_sens
LocID <- LocID[ , -3]


# # Convertir les data.tables en data.frames si nécessaire
# ref_df_all <- as.data.frame(ref_df_all)
# calibratedSensorsAlltime <- as.data.frame(dataout)


head(ref_df_all)
head(uBss_PMS_calibratedSensorsAlltime)

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

subset(uBss_PMS_calibratedSensorsAlltime, PM2.5 == 0)
colocated_calibratedSensorsAlltime <- uBss_PMS_calibratedSensorsAlltime

# Liste des valeurs à garder dans la colonne ID
values_to_keep <- c("Antwerp_40499C", "Antwerp_4043B1", "Antwerp_4049A6", 
                    "Antwerp_4043A7", "Antwerp_40499F", "Antwerp_4043AE", 
                    "Antwerp_4067B3", "Antwerp_40642B", "Antwerp_4047D7", 
                    "Antwerp_4065EA", "Antwerp_4067BD", "Antwerp_4065DA")

# Filtrer les lignes en fonction des valeurs de la colonne ID
colocated_calibratedSensorsAlltime <- colocated_calibratedSensorsAlltime %>%
  filter(ID %in% values_to_keep)

colocated_calibratedSensorsAlltime <- merge(colocated_calibratedSensorsAlltime, LocID, by = "ID", all.x = TRUE)

save(colocated_calibratedSensorsAlltime,file=paste0(outdir,"PMS_colocated_calibratedSensorsAlltime"))


# Définir les stations de référence et leurs capteurs colocalisés
stations_sensors <- list(
  "ANT_REF_R801" = c("40499C", "4043B1"),
  "ANT_REF_R802" = c("4049A6", "4043A7"),
  "ANT_REF_R804" = c("40499F", "4043AE"),
  "ANT_REF_R805" = c("4067B3"),
  "ANT_REF_R811" = c("40642B"),
  "ANT_REF_R817" = c("4047D7"),
  "ANT_REF_M802" = c("4065EA"),
  "ANT_REF_R803" = c("4067BD"),
  "ANT_REF_AL01" = c("4065DA")
)


ref_df_all <- ref_df_all %>% rename(date = datetime)


# dataframe pour une station de référence
create_station_df <- function(station_id, sensor_ids, ref_df, pms_df) {
  station_ref_df <- ref_df %>% filter(ID == station_id)
  for (sensor_id in sensor_ids) {
    sensor_data <- pms_df %>% filter(ID == paste0("Antwerp_", sensor_id)) %>% 
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


output_dir_RefSensData <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration"
if (!dir.exists(output_dir_RefSensData)) {
  dir.create(output_dir_RefSensData, recursive = TRUE)
}


# # Sauvegarder les dataframes
# for (i in seq_along(station_dfs)) {
#   station_name <- names(station_dfs)[i]
#   save(station_dfs[[i]], file = paste0(output_dir_RefSensData, "/", station_name, ".Rda"))
# }


REF_R801_data <- station_dfs[[1]]
REF_R802_data <- station_dfs[[2]]
REF_R804_data <- station_dfs[[3]]
REF_R805_data <- station_dfs[[4]]
REF_R811_data <- station_dfs[[5]]
REF_R817_data <- station_dfs[[6]]
REF_M802_data <- station_dfs[[7]]
REF_R803_data <- station_dfs[[8]]
REF_AL01_data <- station_dfs[[9]] 


save(REF_R801_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R801_data.Rda"))
save(REF_R802_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R802_data.Rda"))
save(REF_R804_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R804_data.Rda"))
save(REF_R805_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R805_data.Rda"))
save(REF_R811_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R811_data.Rda"))
save(REF_R817_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R817_data.Rda"))
save(REF_M802_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_M802_data.Rda"))
save(REF_R803_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_R803_data.Rda"))
save(REF_AL01_data, file = paste0(output_dir_RefSensData, "/uBss_PMS_REF_AL01_data.Rda"))


## two R files with functions to source the SensorIneris_Toolbox.R, usually in the OneDrive - INERIS/SensorIneris/RScript folder
source(choose.files(caption = "Select SensorIneris_Toolbox.R file"))
source(choose.files(caption = "Select uBss and uCi.R file"))


## Define size of the output graphs
WidthTimeplot <- 20
HeightTimeplot <- 18
WidthEtalonnage <- 20
HeightEtalonnage <- 22




path_corr <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/2uBss_correlation_plots2500"
path_timeseries <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/2uBss_TimeSeries_plots2500"

## replace NaN with NA in the subset database
MyDataFrame <- c()
MyDataFrame[is.nan.data.frame(MyDataFrame)] <- NA


# 
#                                 #############
#                                 # timeplot  #
#                                 #############
# 
# 
# timePlot(mydata = REF_R801_data #Directly pass the dataframe to the function
#          , pollutant = c("Ref.PM2.5","PM2.5_40499C", "PM2.5_4043B1") #c(,) list containing the reference in first line then the sensors
#          , plot.type = "l"
#          , lwd = 1.5
#          , group = FALSE
#          , main = ""
#          , ylab = ""
#          , name.pol = c("Ref.PM2.5","PM2.5_40499C", "PM2.5_4043B1") #Name to be print on the plot, can be different from the variable ReferenceAndSensors
#          , auto.text = FALSE
#          , date.format = "%d/%m" #define in line 28-33
#          , cols = c("red","black","blue") #define in line 20-26
#          , key = TRUE
#          , key.columns = 2
#          , key.position = "top"
#          , y.relation = "free")
# 
# dev.copy(png, filename = file.path(path_timeseries, paste0("Time series REF_R801 40499C-4043B1 .png"))
#          , units = "cm", res = 1024, width = WidthTimeplot, height = HeightTimeplot)
# dev.off()
# 
# 
# 
#
# 
#                                             ######################## 
#                                             #   Correlation plots  #
#                                             ########################
# 
# 
# # Correlation REF_R801-40499C
# 
# 
# Limit.XY <- Etalonnage(x = REF_R801_data[, "Ref.PM2.5"] #point to the reference data column
#                        , s_x = NULL
#                        , y = REF_R801_data[, "PM2.5_40499C"]  #point to the sensor data column
#                        , s_y = NULL
#                        , AxisLabelX = "PM2.5_FIDAS200_R801" #Name to be print on the X axis, can be different from the variable name
#                        , AxisLabelY = "PM2.5_PMS5003_40499C" #Name to be print on the Y axis, can be different from the variable name
#                        , Title = ""
#                        , Marker = 19
#                        , Couleur = "blue"
#                        , ligne = "p" 
#                        , XY_same = FALSE 
#                        , lim = NULL # ? 
#                        , steps = c(10, 10) 
#                        , digitround = NULL
#                        , marges = NULL
#                        , PlotAxis = "s"
#                        , OrdonneeOrigine = NULL)
# 
# ## Add the X=Y line to ease the comparison between graphs
# lines(x= c(min(Limit.XY),max(Limit.XY)), y=c(min(Limit.XY),max(Limit.XY)), type = "l", col = "green4")
# mtext(paste0("Line Y=X "),line=-36.3,adj=1,padj=0,col= "green4",cex=1.2)
# 
# Cal_Line(x = REF_R801_data[, "Ref.PM2.5"] #point to the reference data column
#          , s_x = NULL
#          , y = REF_R801_data[, "PM2.5_40499C"] #point to the sensor data column
#          , s_y = NULL
#          , Mod_type = "Linear"
#          , Matrice = NULL
#          , line_position = -1.3
#          , Couleur = "red"
#          , Sensor_name = NULL
#          , f_coef1 = "%.2f"
#          , f_coef2 = "%.2f"
#          , f_R2 = "%.3f"
#          , lim = Limit.XY #Output from the Etalonnage function lines 66-82
#          , marges = NULL
#          , Covariates = NULL
#          , Equation = "RMSE")
# 
# dev.copy(png, filename = file.path(path_corr, paste0(" Correlation REF_R801-40499C.png"))
#          , units = "cm", res = 1024, width = WidthEtalonnage, height = HeightEtalonnage)
# dev.off()
# 
# 
#                                           
# 
# 
# # Correlation REF_R801-4043B1
# 
# Limit.XY <- Etalonnage(x = REF_R801_data[, "Ref.PM2.5"] #point to the reference data column
#                        , s_x = NULL
#                        , y = REF_R801_data[, "PM2.5_4043B1"]  #point to the sensor data column
#                        , s_y = NULL
#                        , AxisLabelX = "PM2.5_FIDAS200_R801" #Name to be print on the X axis, can be different from the variable name
#                        , AxisLabelY = "PM2.5_PMS5003_4043B1" #Name to be print on the Y axis, can be different from the variable name
#                        , Title = ""
#                        , Marker = 19
#                        , Couleur = "blue"
#                        , ligne = "p" 
#                        , XY_same = FALSE 
#                        , lim = NULL # ? 
#                        , steps = c(10, 10) 
#                        , digitround = NULL
#                        , marges = NULL
#                        , PlotAxis = "s"
#                        , OrdonneeOrigine = NULL)
# 
# ## Add the X=Y line to ease the comparison between graphs
# lines(x= c(min(Limit.XY),max(Limit.XY)), y=c(min(Limit.XY),max(Limit.XY)), type = "l", col = "green4")
# mtext(paste0("Line Y=X "),line=-36.3,adj=1,padj=0,col= "green4",cex=1.2)
# 
# Cal_Line(x = REF_R801_data[, "Ref.PM2.5"] #point to the reference data column
#          , s_x = NULL
#          , y = REF_R801_data[, "PM2.5_4043B1"] #point to the sensor data column
#          , s_y = NULL
#          , Mod_type = "Linear"
#          , Matrice = NULL
#          , line_position = -1.3
#          , Couleur = "red"
#          , Sensor_name = NULL
#          , f_coef1 = "%.2f"
#          , f_coef2 = "%.2f"
#          , f_R2 = "%.3f"
#          , lim = Limit.XY #Output from the Etalonnage function lines 66-82
#          , marges = NULL
#          , Covariates = NULL
#          , Equation = "RMSE")
# 
# dev.copy(png, filename = file.path(path_corr, paste0(" Correlation REF_R801-4043B1.png"))
#          , units = "cm", res = 1024, width = WidthEtalonnage, height = HeightEtalonnage)
# dev.off()
# 





load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R801_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R802_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R804_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R805_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R811_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R817_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_M802_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_R803_data.Rda")
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/df_correlations_timeplots_RefSensData_ALL/after_calibration/uBss_PMS_REF_AL01_data.Rda")

plots_generator <- function(station_data, station_id, sensor_ids, path_corr, path_timeseries) {
  
  station_id_short <- gsub("_data$", "", station_id)
  
  colors_warehouse <- c("red","black","blue","purple3","green4","gold","pink4")
  needed_colors <- colors_warehouse[1:(length(sensor_ids) + 1)]
  
  # graphique de séries temporelles
  timePlot(
    mydata = station_data,
    pollutant = c("PM2.5", paste0("PM2.5_", sensor_ids)), # "Ref.PM2.5", "PM2.5_40499C", "PM2.5_4043B1"
    plot.type = "l",
    lwd = 1.5,
    group = FALSE,
    main = "",
    ylab = "",
    name.pol = c(paste0("PM2.5_FIDAS200_", station_id_short), paste0("PM2.5_PMS5003_", sensor_ids)), # Nom affiché sur le graphique
    auto.text = FALSE,
    date.format = "%d/%m",
    cols = needed_colors, # Utiliser les couleurs dynamiques
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


# Liste des stations et capteurs colocalisés
stations_sensors_data_IDs <- list(
  "REF_R801_data" = c("40499C", "4043B1"),
  "REF_R802_data" = c("4049A6", "4043A7"),
  "REF_R804_data" = c("40499F", "4043AE"),
  "REF_R805_data" = c("4067B3"),
  "REF_R811_data" = c("40642B"),
  "REF_R817_data" = c("4047D7"),
  "REF_M802_data" = c("4065EA"),
  "REF_R803_data" = c("4067BD"),
  "REF_AL01_data" = c("4065DA")
)


# Parcourir chaque station et générer les graphiques
for (station_id in names(stations_sensors_data_IDs)) {
  sensor_ids <- stations_sensors_data_IDs[[station_id]]
  station_data <- get(station_id)
  plots_generator(station_data, station_id, sensor_ids, path_corr, path_timeseries)
}


