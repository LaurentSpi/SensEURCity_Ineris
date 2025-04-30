##############################################
#            GLOBAL SETTINGS FILE            #
#        For ALT_SensEURCity project         #
#     Centralizes all paths and parameters   #
##############################################

# Root project directory - CHANGE THIS TO YOUR PATH
path_project_root <- "C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity"

#######################
#  PATHS DEFINITIONS  #
#######################

# INPUTS and OUTPUTS general
path_inputs_general  <- file.path(path_project_root, "INPUTS/")
path_outputs_general <- file.path(path_project_root, "OUTPUTS/")

# FIGURES paths
path_figures_general <- file.path(path_outputs_general, "figs/")
path_figures_outliers <- file.path(path_figures_general, "outliers_detection/")
path_figures_outliers_test <- file.path(path_figures_general, "outliers_detection TEST/")

# Specific output paths
path_correlation_outputs <- file.path(path_outputs_general, "df_correlations_timeplots_RefSensData_ALL/")
path_correlation_after_calibration <- file.path(path_correlation_outputs, "after_calibration/")
path_correlation_plots <- file.path(path_correlation_after_calibration, "correlation_plots/")
path_timeseries_plots <- file.path(path_correlation_after_calibration, "timeSeries_plots/")

# Dataset directory for raw data files
path_dataset <- file.path(path_project_root, "dataset/")

##############################
#  FILES FOR EACH PROCESS    #
##############################

# INPUT FILES
file_metadata_sites          <- file.path(path_inputs_general, "metadata/metadata_sites.csv")
file_station_typology        <- file.path(path_inputs_general, "Stations_RIVM_LML_Feb2020_format_modif.csv")
file_ref_df_all_csv          <- file.path(path_inputs_general, "ref_df_all.csv")
file_ref_df_all_rda          <- file.path(path_inputs_general, "ref_df_all.Rda")
file_CLC_legend_csv          <- file.path(path_inputs_general, "SIG_data/legend_CLC_V4.csv")
file_CLC_raster_tif          <- file.path(path_inputs_general, "SIG_data/CLC_Netherlands_NT.tif")
file_shapefile_belgium       <- file.path(path_inputs_general, "gadm41_BEL_2.shp")
file_lcs_df_all_csv          <- file.path(path_inputs_general, "LCS_df_all.csv")
file_lcs_df_all_rda          <- file.path(path_inputs_general, "LCS_df_all.Rda")

# OUTPUT FILES (GENERATED)
file_LCS_df_all_clean_Rda         <- file.path(path_outputs_general, "LCS_df_all_clean.Rda")
file_typo_CLC_BDD_comparison_Rda  <- file.path(path_outputs_general, "typo_CLC_BDD_comparison.Rda")
file_typo_CLC_BDD_comparison_csv  <- file.path(path_outputs_general, "typo_CLC_BDD_comparison.csv")
file_ref_df_all_wtypo_Rda         <- file.path(path_outputs_general, "ref_df_all_wtypo.Rda")
file_LCS_df_all_clean_wtypo_Rda   <- file.path(path_outputs_general, "LCS_df_all_clean_wtypo.Rda")
file_typology_sens_Rda            <- file.path(path_outputs_general, "typology_sens.Rda")
file_LCS_df_all_clean_groups_Rda  <- file.path(path_outputs_general, "LCS_df_all_clean_groups.Rda")
file_LCS_df_all_clean_groups_outliers_Rda <- file.path(path_outputs_general, "LCS_df_all_clean_groups_outliers.Rda")
file_calibratedSensorsAlltime_Rda <- file.path(path_outputs_general, "calibratedSensorsAlltime.Rda")
file_calibrationFactorsAlltime_csv <- file.path(path_outputs_general, "calibrationFactorsAlltime_alltime_nmax1000_distmaxRepmax_outliers.csv")
file_calibratedSensorsAlltime_csv  <- file.path(path_outputs_general, "calibratedSensorsAlltime_alltime_nmax1000_distmaxRepmax_outliers.csv")
file_colocated_calibratedSensorsAlltime_Rda <- file.path(path_outputs_general, "colocated_calibratedSensorsAlltime.Rda")
file_colocated_LCS_df_all_clean_Rda <- file.path(path_correlation_outputs, "colocated_LCS_df_all_clean.Rda")

# OUTPUT FILES (CORRELATION/REF-SENSORS)
# Liste des fichiers pour les données de référence et capteurs
files_ref_sensor_data_list <- list(
  REF_R801_data = file.path(path_correlation_after_calibration, "REF_R801_data.Rda"),
  REF_R802_data = file.path(path_correlation_after_calibration, "REF_R802_data.Rda"),
  REF_R804_data = file.path(path_correlation_after_calibration, "REF_R804_data.Rda"),
  REF_R805_data = file.path(path_correlation_after_calibration, "REF_R805_data.Rda"),
  REF_R811_data = file.path(path_correlation_after_calibration, "REF_R811_data.Rda"),
  REF_R817_data = file.path(path_correlation_after_calibration, "REF_R817_data.Rda"),
  REF_M802_data = file.path(path_correlation_after_calibration, "REF_M802_data.Rda"),
  REF_R803_data = file.path(path_correlation_after_calibration, "REF_R803_data.Rda"),
  REF_AL01_data = file.path(path_correlation_after_calibration, "REF_AL01_data.Rda")
)

# Configuration des stations et capteurs
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

##############################
#  VARIABLES COMMON          #
##############################

# Pollutant
pollutant_name <- "PM25"

# Location
location_name <- "Antwerp"

# Calibration parameters
calibration_params <- list(
  useBootstrap = TRUE,
  nBootstrapSamples = 100,
  referenceStationFractionAfterResampling = 0.8,
  minCountSensor = 2,
  maxCountSensor = 1000
)

# Figure size parameters
figure_sizes <- list(
  WidthTimeplot = 20,
  HeightTimeplot = 18,
  WidthEtalonnage = 20,
  HeightEtalonnage = 22
)

# Création des dossiers s'ils n'existent pas
create_dirs <- function() {
  dirs <- c(
    path_outputs_general,
    path_figures_general,
    path_figures_outliers,
    path_figures_outliers_test,
    path_correlation_outputs,
    path_correlation_after_calibration,
    path_correlation_plots,
    path_timeseries_plots
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Directory created:", dir, "\n")
    }
  }
}

# Appel de la fonction pour créer les dossiers
create_dirs()

