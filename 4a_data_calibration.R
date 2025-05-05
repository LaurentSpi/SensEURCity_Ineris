rm(list = ls())

# Charger le fichier de configuration global
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

# Import libraries
library(tidyverse)
library(raster)
library(sf)
library(RColorBrewer)
library(fields)
library(data.table)
library(chron)

#################################################################################
#                                  CALIBRATION                                  #
#                               Created 28/09/2021                              #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################

shapefile <- st_read(file_shapefile_belgium)
st_crs(shapefile) <- "EPSG:4326"

# Directory paths
# indir   <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/INPUTS/"# path for input directory
# outdir  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/OUTPUTS/"# path for output directory
# outdir2  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/OUTPUTS/figs/"# path for output directory
# outdir3 <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity/OUTPUTS/figs/outliers_detection"

# Init variables
loc <-"Antwerp" # estimation location
pol <-"PM25" # pollutant


# Init calibration
useBootstrap = calibration_params$useBootstrap
nBootstrapSamples = calibration_params$nBootstrapSamples  # Should be large... Not used if useBootstrap = FALSE
referenceStationFractionAfterResampling = calibration_params$referenceStationFractionAfterResampling  # After resampling/bootstrapping
if(!useBootstrap) nBootstrapSamples = 1  # Just one calculation in case of no bootstrapping

# rje3 hadi !!  select_time_start=as.POSIXct("2022-01-10 00:00:00", origin="1970-01-01",tz="CET") # To be changed if necessary
# rje3 hadi !!  select_time_end=as.POSIXct("2022-01-19 00:00:00", origin="1970-01-01",tz="CET")

#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(file_LCS_df_all_clean_groups_outliers_Rda)
sensData <- LCS_df_all_clean_groups_outliers


# rje3 hadi !!  sensData <- sensData[which(sensData$datetime>=select_time_start & sensData$datetime<=select_time_end),]
sensData$PM2.5[which(sensData$outliers=="outlier")]<-NA
sensData=sensData[complete.cases(sensData$PM2.5),]
sensData2=subset(sensData,select=-c(Typology,Clust,Season,Group,outliers,DayType,Periods))
sensData2$ID=as.character(sensData2$ID)


load(file_ref_df_all_rda)
allreferenceData1=ref_df_all

# allreferenceData1 <- allreferenceData1 %>%
#   rename(
#     PM2.5 = Ref.PM2.5,
#     Y = Ref.Lat,
#     X = Ref.Long,
#     datetime = date
#   )


allreferenceData1=subset(allreferenceData1,select=-c(PM10))


load(file_typo_CLC_BDD_comparison_Rda)
allreferenceData <- allreferenceData1


colnames(allreferenceData)
colnames(typo_comparison)

typo_comparison <- typo_comparison %>% rename(ID = Location.ID)

allreferenceData <- left_join(allreferenceData, typo_comparison[, c("ID", "Typology.sEURcity")], by = "ID")
names(allreferenceData)[names(allreferenceData) == "Typology.sEURcity"] <- "Typology"


allreferenceData$Representativity_max <- ifelse(allreferenceData$Typology %in% c("INDUS", "URB"), 
                                                2000, 
                                                ifelse(allreferenceData$Typology == "TRA", 100, NA))

allreferenceData2=subset(allreferenceData,select=-c(Typology))

# allreferenceData <- allreferenceData %>%
#   rename(
#     ID = Location.ID
#   )
# 
# allreferenceData2 <- allreferenceData2 %>%
#   rename(
#     ID = Location.ID
#   )
#####################################
#            DEFINE TIME            #          
#####################################

start_date <- min(sensData$datetime) # starting date
end_date <- max(sensData$datetime) # ending date
timeout <- seq(as.POSIXct(start_date), as.POSIXct(end_date), by="hour")
nbr_hours <- length(timeout)
nbr_hours

#####################################
#            CALIBRATION            #          
#####################################
# Run script
source(choose.files(caption = "Select interpolate.R file"))
# source(choose.files(caption = "uBss and uCi.R file"))
#source("interpolate.R")

sensData2 <- as.data.frame(sensData2)
allreferenceData2 <- as.data.frame(allreferenceData2)
duplicates <- duplicated(sensData2[, c("ID", "datetime")])

# Garder uniquement les lignes non dupliquées, car au fil de l'execution du code 
# il s'est avéré que, pour une raison que j'ignores , ça nous arrive d'avoir des doublons 
# i mean differentes lignes pour un meme capteur à un meme instant donné 
# ce qui n'a pas lieu d'etre normalement, et ceci nous pose problème dans la ligne "rownames(nsensData) = nsensData$ID" on a donc des lignes avec le memes noms ( memes ID)
# ce qui n'est pas acceptable donc R car chaque ligne avoir un unique nom propre à elle  
sensData2 <- sensData2[!duplicates, ]


uBss <- function(DataFrame, Sensors, DateInit, DateEnd){
  ## This function return the uncertainty between sensor systems as described in the draft of the sensor evaluation protocol from the CEN-TC264-WG42
  ## DataFrame = general dataframe containing the dataset
  ## Sensors = list of the sensors name as c("Sensor1","Sensor2", ...)
  ## DateInit = date of the begining of the campaign as "2020-02-03 12:00:00 CET"
  ## DateEnd = date of the inding of the campaign as "2020-02-03 12:00:00 CET"
  
  uBss.df <- DataFrame
  uBss.df <- subset(uBss.df, uBss.df$date >= DateInit & uBss.df$date <= DateEnd)
  uBss.df <- uBss.df[, Sensors]
  uBss.df$ym <- rowMeans(uBss.df[, Sensors], na.rm = TRUE)
  for (i in 1:length(Sensors)) {
    y_ym <- as.data.frame(uBss.df[, Sensors[i]] - uBss.df$ym)
    colnames(y_ym) <- paste0("y",i,"_ym")
    SumSqY <- as.data.frame((uBss.df[, Sensors[i]] - uBss.df$ym)^2)
    colnames(SumSqY) <- paste0("SumSqY",i)
    uBss.df <- cbind(uBss.df, y_ym, SumSqY)
  }
  SumSqYList <- colnames(uBss.df[grep("SumSqY", colnames(uBss.df))])
  uBss.df$sumYsq <- rowSums(uBss.df[, SumSqYList], na.rm = TRUE)
  uBss<- sqrt(sum(uBss.df$sumYsq, na.rm = TRUE)/(nrow(uBss.df)*(length(Sensors)-1)))
  return(uBss)
}


calibrationFactorsAlltime <- data.frame()
calibratedSensorsAlltime <- data.frame()

# Loop over hours
for (ntimes in 0:nbr_hours) {
  
  print(paste0("Je suis à l'itération ", ntimes))
  
  ndatetime = timeout[ntimes]
  char_time = as.character(ndatetime)
  char_time = gsub("-","",char_time); char_time = gsub(":","",char_time)
  char_time = gsub(" ","",char_time)
  char_time = substr(char_time, 1, 10)
  
  print(char_time)
  
  ###################
  ### Sélection des données ###
  
  # Données des capteurs
  nsensData = sensData2[which(sensData2$datetime == ndatetime),]
  nsensData$ID = as.character(nsensData$ID)
  nsensData = nsensData[complete.cases(nsensData), ]
  
  if (length(nsensData[, 1]) > 0) {
    
    rownames(nsensData) = nsensData$ID
    numberOfSensors = nrow(nsensData)
    
    # Données des stations de référence
    referenceData = allreferenceData2[which(allreferenceData2$datetime == ndatetime),]    
    referenceData = referenceData[referenceData$PM2.5 > 0.0,]
    referenceData = referenceData[complete.cases(referenceData), ]
    
    rownames(referenceData) = referenceData$ID
    numberOfReferenceStations = nrow(referenceData)
    
    #################################
    ### Calcul de la matrice des distances ###
    
    # Calculer la matrice des distances entre stations de référence et capteurs
    distancesSensorReference = data.frame()
    for (referenceInt in 1:nrow(referenceData)) {
      referenceRow = referenceData[referenceInt,]
      currentRow = t(data.frame(
        sqrt((nsensData$X - referenceRow$X)^2 + (nsensData$Y - referenceRow$Y)^2)
      ))
      colnames(currentRow) = nsensData$ID
      rownames(currentRow) = referenceRow$ID
      distancesSensorReference = rbind(distancesSensorReference, currentRow)
    }
    
    ############################
    ### Définition des groupes de capteurs ###
    
    # Sélection des groupes de capteurs autour des stations de référence
    minCountSensor <- 2
    maxCountSensor <- 1000
    listSensorCalibrationGroups <- list()
    vectorGroupIDs <- c()
    allCloseSensors <- c()
    iList <- 1
    
    # # Liste des capteurs colocalisés à exclure
    CloseSensors <- c("40499C", "4043B1", "4049A6", "4043A7", "4065EA", "4067BD", "40499F", "4043AE", "4067B3", "40642B", "4047D7", "4065DA")
    CloseSensors <- paste0("Antwerp_", CloseSensors)
    
    
    for (iRef in 1:nrow(referenceData)) {
      maxTolDistSensor <- referenceData$Representativity_max[iRef]
      referenceID <- as.character(referenceData[iRef, ]$ID)
      
      # indices des capteurs proches
      valid_indices <- which(distancesSensorReference[iRef, ] <= maxTolDistSensor)
      
      if (length(valid_indices) > 0) {
        NoCloseSensors <- colnames(distancesSensorReference)[valid_indices]
        
        # Exclusion capteurs colocalisés
        NoCloseSensors <- NoCloseSensors[!NoCloseSensors %in% CloseSensors]
        
        # Limitation du nombre de capteurs
        NoCloseSensors <- intersect(NoCloseSensors[1:min(length(NoCloseSensors), maxCountSensor)], nsensData$ID)
        
        if (length(NoCloseSensors) >= minCountSensor) {
          
          # Calcul `uBss`
          validData <- nsensData[nsensData$ID %in% NoCloseSensors, ]
          tryCatch({
            wideData <- validData %>%
              dplyr::select(ID, datetime, PM2.5) %>%
              tidyr::spread(key = ID, value = PM2.5)
            
            uBss_value <- uBss(wideData, NoCloseSensors, DateInit = ndatetime, DateEnd = ndatetime)
            if (uBss_value <= 3.5) {
              vectorGroupIDs[iList] <- referenceID
              listSensorCalibrationGroups[[iList]] <- list(referenceID, NoCloseSensors)
              allCloseSensors <- c(allCloseSensors, NoCloseSensors)
              iList <- iList + 1
            } else {
              #print(paste("uBss =", uBss_value, "> 3 µg/m³. Groupe rejeté."))
            }
          }, error = function(e) {
            # print(paste("Erreur lors du calcul de uBss :", e$message))
          })
        } else {
          #print("Pas assez de capteurs valides après vérification des colonnes.")
        }
      } else {
        #print(paste("Aucun capteur trouvé dans le rayon pour la station :", referenceID))
      }
    }
    
    
    
    # Vérification finale de allCloseSensors
    allCloseSensors <- unique(allCloseSensors)
    print(vectorGroupIDs)
    print(allCloseSensors)
    print(listSensorCalibrationGroups)
    
    
    # Remove na values for current hour
    nsensData = na.omit(nsensData)
    referenceData = na.omit(referenceData)
    
    hourlySensorValues = as.numeric(nsensData$PM2.5)
    names(hourlySensorValues) = nsensData$ID
    
    hourlyReferenceValues  = as.numeric(referenceData$PM2.5)
    names(hourlyReferenceValues) = referenceData$ID
    
    # Use only those sensors that are part of a local group for calibration
    hourlySensorValues = hourlySensorValues[allCloseSensors]
    hourlySensorValues = sort(unlist(na.omit(hourlySensorValues)))
    nSensors = length(hourlySensorValues)
    hourlySensorValuesForCalibration = hourlySensorValues
    
    ###################
    ### CALIBRATION ###          
    
    numberOfCalibrationGroups = length(listSensorCalibrationGroups)
    
    if(numberOfCalibrationGroups>0){
      # Construct a initial data frame for every sample of calculated calibration factors 
      # in the bootstrap loop
      calibrationFactorPerSampleReset = data.frame(
        "ID" = vectorGroupIDs,
        "X" = referenceData[vectorGroupIDs,]$X,
        "Y" = referenceData[vectorGroupIDs,]$Y,
        "CalibrationFactor" = rep(NA,numberOfCalibrationGroups)
        ,stringsAsFactors = F)
      rownames(calibrationFactorPerSampleReset) = vectorGroupIDs
      
      # Construct a initial data frame for every sample of calibrated sensors 
      # in the bootstrap loop
      calibratedSensorsPerSampleReset = data.frame(
        "ID" = nsensData$ID,
        "X" = as.numeric(nsensData$X),
        "Y" = as.numeric(nsensData$Y),
        "PM2.5" = as.numeric(nsensData$PM2.5),
        "CalibrationFactor" = rep(NA,nrow(nsensData)),
        "CalibratedPM2.5" = rep(NA,nrow(nsensData)),
        "DIFF" = rep(NA,nrow(nsensData))
        ,stringsAsFactors = F)
      rownames(calibratedSensorsPerSampleReset) = calibratedSensorsPerSampleReset$ID
      
      # Contruct an initial data frame that will contain the data of all samples in the bootstrap loop
      # (It will contain redundant X and Y information)
      calibrationFactorAllSamples = data.frame()
      calibratedSensorsAllSamples = data.frame()
      print(' ICI ')
      
      # Start bootstrap loop. (In case of no bootstrapping, this for loop will only consist of one iteration)
      for(iSample in 1:nBootstrapSamples){
        if (iSample == 1){ # no resampling.
          sensorSet = hourlySensorValuesForCalibration
          referenceSet = referenceData
        } else {
          sensorSet  = sample(hourlySensorValuesForCalibration,replace =T)
          #referenceSet = unique(sample(hourlyReferenceValues,replace = T))
          
          nrowRef = nrow(referenceData)
          # Only use a specified fraction of randomly chosen rows of the reference data.
          randomrows = round(nrowRef*referenceStationFractionAfterResampling,0)
          referenceSet = unique(referenceData[sample(nrowRef,randomrows),])
          #referenceSet = unique(sample(referenceData,replace = F))
          
          # The reference data are also reshuffeled, leaving out some reference locations 
          # whose calibration factors will be determined using interplation of the calcalibration factors that 
          # remain after reshuffeling.
          # This will add uncertainty associated with (IDW) interpolation of calibration factors. 
        }
        
        calibrationFactorPerSample = calibrationFactorPerSampleReset
        calibratedSensorsPerSample = calibratedSensorsPerSampleReset
        # loop over Groups of Sensors to determine the calibration factor 
        # at the group (refererence station) location
        
        for(listItem in listSensorCalibrationGroups){
          referenceID = listItem[[1]][1]
          #referenceValue = as.numeric(referenceSet[referenceID]) # returns NA if referenceID not available in sample
          referenceValue = as.numeric(referenceSet[referenceID,]$PM2.5) # returns NA if referenceID not available in sample
          #referenceValue = referenceData[referenceID,]$PM2.5
          
          if(is.na(referenceValue)){ # reference measurement not present in sample
            calibrationFactor = NA
          } else { # reference measurement present in sample
            # Select sensor ID's in calibration Group
            sensorIDs = listItem[[2]]
            
            # select those sensors which are in the calibration group and are still available after
            # after resampling the data frame (for purpose of bootstrapping) 
            sensorIDs = sensorIDs[sensorIDs %in% names(sensorSet)]
            
            # Select sensor measurements, corresponding to the selected sensorID's 
            # keeping in mind that, because of resampling, a particular sensorID and corresponding 
            # measurement value can occur multiple times 
            sensorGroupValues = na.omit(as.numeric(sensorSet[names(sensorSet) %in% sensorIDs]))
            
            ################## Actual calibration #####################
            if (length(sensorGroupValues) < minCountSensor){
              calibrationFactor = NA # Don't trust this calibration factor
            } else {
              calibrationFactor = referenceValue/mean(sensorGroupValues)
            }
          } # end reference measurement present in sample
          # Store the calibration factors
          
          calibrationFactorPerSample[referenceID,]$CalibrationFactor = calibrationFactor
          ################## End actual calibration #####################
          
        } # loop over sensorGroups
        
        # Replace NA calibration factors by interpolation of valid values
        calibrationFactorsForInterpolation = calibrationFactorPerSample[!is.na(calibrationFactorPerSample$CalibrationFactor),]
        
        nonValidcalibrationFactorPerSample =  calibrationFactorPerSample[is.na(calibrationFactorPerSample$CalibrationFactor),]
        nrows = nrow(nonValidcalibrationFactorPerSample)
        
        if(nrows>0){
          for (irow in 1:nrows){
            nonValidSampleGroupCalibrationFactor = nonValidcalibrationFactorPerSample[irow,]
            calibrationFactorPerSample[nonValidSampleGroupCalibrationFactor$ID,]$CalibrationFactor =
              Interpolate_REFs(
                nonValidSampleGroupCalibrationFactor$X,
                nonValidSampleGroupCalibrationFactor$Y
              )
          }
        }
        # End replacing NA calibration factors
        
        # Start calibrating all sensors by interpolating the calibration factors at sensor locations
        nrows = nrow(calibratedSensorsPerSample)
        if(nrows>0){
          for (irow in 1:nrows){
            sensorRow = calibratedSensorsPerSample[irow,]
            calibratedSensorsPerSample[sensorRow$ID,]$CalibratedPM2.5 =
              calibratedSensorsPerSample[sensorRow$ID,]$PM2.5 *
              Interpolate_REFs(
                sensorRow$X,
                sensorRow$Y
              )
            calibratedSensorsPerSample[sensorRow$ID,]$CalibrationFactor =
              Interpolate_REFs(
                sensorRow$X,
                sensorRow$Y
              )
            
            calibratedSensorsPerSample[sensorRow$ID,]$DIFF =
              calibratedSensorsPerSample[sensorRow$ID,]$CalibratedPM2.5 - 
              calibratedSensorsPerSample[sensorRow$ID,]$PM2.5
            
          }
        }
        # End calibrating all sensors by interpolating the calibration factors at sensor locations
        
        calibrationFactorAllSamples = rbind(calibrationFactorAllSamples,calibrationFactorPerSample)
        calibratedSensorsAllSamples = rbind(calibratedSensorsAllSamples,calibratedSensorsPerSample)
        
        if (iSample == 1){ # no resampling, store the average calibration factors.
          calibrationFactors = calibrationFactorAllSamples
          calibratedSensors = calibratedSensorsAllSamples
        }
        #print(paste("Finished ",iSample," of ",nBootstrapSamples) )
      } # end bootstrap loop
      print(' ICI 2 ')
      
      # In case of bootstrapping, attach uncertainties
      if (nBootstrapSamples > 1) {
        calibrationFactors = merge(
          calibrationFactors,
          setNames(
            aggregate(
              . ~ ID,
              calibrationFactorAllSamples[, c("ID", "CalibrationFactor")],
              FUN  = function(x)
                c(quantile(x, 0.05, na.rm = T),
                  quantile(x, 0.95, na.rm = T)),
              na.action = na.pass
            ),
            c("ID", "Perc")
          ),
          by.x = "ID",
          by.y = "ID" ,
          all.x = T
        )
        print(' ICI 2 *  ')
        
        calibratedSensors = merge(
          calibratedSensors,
          setNames(
            aggregate(
              . ~ ID,
              calibratedSensorsAllSamples[, c("ID", "CalibratedPM2.5")],
              FUN  = function(x)
                c(quantile(x, 0.05, na.rm = T),
                  quantile(x, 0.95, na.rm = T)),
              na.action = na.pass
            ),
            c("ID", "Perc")
          ),
          by.x = "ID",
          by.y = "ID" ,
          all.x = T
        )
        print(' ICI 3 ')
        
        fileOutName = file.path(path_outputs_general, paste0("calibrationFactors_alltime_nmax", maxCountSensor, "_distmaxRepmax_outliers"))
        fileOutName2 = file.path(path_figures_general, paste0("calibrationFactors_alltime_nmax", maxCountSensor, "_distmaxRepmax_outliers"))
        fileOutNameSensor = file.path(path_outputs_general, paste0("calibratedSensors_alltime_nmax", maxCountSensor, "_distmaxRepmax_outliers"))
        
      } else {
        fileOutName = file.path(path_outputs_general, "calibrationFactors")
        fileOutName2 = file.path(path_figures_general, "calibrationFactors")
        fileOutNameSensor = file.path(path_outputs_general, "calibratedSensors")
        
      }
      
      calibrationFactors$datetime = rep(ndatetime, length(calibrationFactors[, 1]))
      calibratedSensors$datetime = rep(ndatetime, length(calibratedSensors[, 1]))
      
      # write.csv(calibrationFactors,paste0(fileOutName,"_",char_time,".csv"),row.names = F)
      # write.csv(calibratedSensors,paste0(fileOutNameSensor,"_",char_time,".csv"),row.names = F)
      
      # Concatenate
      if (ntimes == 1) {
        calibrationFactorsAlltime = calibrationFactors
        calibratedSensorsAlltime = calibratedSensors
      } else{
        calibrationFactorsAlltime = rbind(calibrationFactorsAlltime, calibrationFactors)
        calibratedSensorsAlltime = rbind(calibratedSensorsAlltime, calibratedSensors)
      }
      
    }
    # Plot results
    # library(rgdal)
    # library(ggplot2)
    #
    # provinceNL <- readOGR("C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SensEURCity/SENSEURCITY_CAL/shapeIn/nederlandProvinciesLines.shp")
    # provinceNL.df <- fortify(provinceNL)
    # colnames(provinceNL.df)[which(names(provinceNL.df) == "long")] <- "X"
    # colnames(provinceNL.df)[which(names(provinceNL.df) == "lat")]  <- "Y"
    # p = ggplot() +
    #   geom_path(data = provinceNL.df,aes(x=X,y=Y,group = group)) +
    #   geom_point(data = calibrationFactors, aes(x =X, y =Y, color = CalibrationFactor  ) ) +
    #   coord_fixed() +
    #   scale_color_gradientn(colours = rev(rainbow(10)),limits = c(0,4)) +
    #   theme_bw()
    # ggsave(p , file = paste0(fileOutName2,"_",char_time,".png"), width = 4, height = 3, device = "png")
    
  } #if sensor data exist
  
}

# 
# # Charger les données géographiques
# provinceBEL <- st_read("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity/INPUTS/gadm41_BEL_4.shp")
# province_ANT <- provinceBEL[provinceBEL$NAME_2 == "Antwerpen", ]
# 
# print(st_crs(province_ANT))
# print(st_crs(calibrationFactors))
# 
# # Plot des calibration factors
# # Définir le CRS des données de calibration (EPSG:32631)
# calibrationFactors_sf <- st_as_sf(calibrationFactors, coords = c("X", "Y"), crs = 32631)
# 
# # Reprojeter la géométrie de province_ANT dans le CRS des données de calibration (EPSG:32631)
# province_ANT_projected <- st_transform(province_ANT, crs = st_crs(calibrationFactors_sf))
# 
# # Vérifier que les deux ont le même CRS
# print(st_crs(province_ANT_projected))
# print(st_crs(calibrationFactors_sf))
# 
# # Palette des couleurs pour les graphes
# colors_palette <- brewer.pal(9, "YlGnBu")
# 
# # Plotter la carte avec les données reprojetées
# plooot <- ggplot() +
#   geom_sf(data = province_ANT_projected, color = "black", fill = NA, size = 0.5) + # Ajoute la frontière de la province
#   geom_sf(data = calibrationFactors_sf, aes(color = CalibrationFactor), size = 3) + # Points des capteurs
#   scale_color_gradientn(colors = colors_palette,  # Utilisation de la palette YlGnBu
#                         limits = range(calibrationFactors$CalibrationFactor, na.rm = TRUE),
#                         na.value = "black") +
#   theme_bw() +
#   labs(title = "Province d'Antwerp avec Points de Calibration",
#        color = "Facteur de Calibration")
# print(plooot)
# 
# # Sauvegarder la carte en haute résolution avec une taille plus grande
# ggsave(file = paste0(fileOutName2, "_NoLocali0_", char_time, ".png"), plot = plooot, width = 10, height = 8, dpi = 300)
# 
# 
# 
# 
# # Plot des calibrated sensors
# # Définir le CRS des données de calibration (EPSG:32631)
# calibratedSensors_sf <- st_as_sf(calibratedSensors, coords = c("X", "Y"), crs = 32631)
# 
# # Reprojeter la géométrie de province_ANT dans le CRS des données de calibration (EPSG:32631)
# province_ANT_sensors <- st_transform(province_ANT, crs = st_crs(calibratedSensors_sf))
# 
# # Vérifier que les deux ont le même CRS
# print(st_crs(province_ANT_sensors))
# print(st_crs(calibratedSensors_sf))
# 
# 
# # Plotter la carte avec les données reprojetées
# 
# plot_sensor <- ggplot() +
#   geom_sf(data = province_ANT_sensors, color = "black", alpha = 0.0, size = 3) +
#   geom_sf(data = calibratedSensors_sf, aes(color = CalibrationFactor), size = 3) +
#   scale_color_gradientn(colors = colors_palette,  # Utilisation de la palette YlGnBu
#                         limits = range(calibrationFactors$CalibrationFactor, na.rm = TRUE),
#                         na.value = "black") +
#   theme_bw() +
#   labs(title = "Province d'Antwerp avec Points de Calibration",
#        color = "Facteur de Calibration")
# 
# # Afficher le graphique
# print(plot_sensor)
# # Sauvegarder la carte en haute résolution avec une taille plus grande
# ggsave(file = paste0(fileOutName2, "_NoLocali1_", char_time, ".png"), plot = plot_sensor, width = 10, height = 8, dpi = 300)
# 
# 
# # DIFF est la différence entre les valeurs calibrées et non calibrées de PM2.5
# calibratedSensors$DIFF <- calibratedSensors$CalibratedPM2.5 - calibratedSensors$PM2.5
# 
# 
# # Conversion de calibratedSensors en un objet sf avec les coordonnées X et Y
# calibratedSensors_sf <- st_as_sf(
#   calibratedSensors,
#   coords = c("X", "Y"),
#   crs = 32631  # Remplacez 32631 par le CRS de vos données si différent
# )
# 
# 
# plot_sens <- ggplot() +
#   geom_sf(data = province_ANT_projected, color = "black", fill = NA, size = 0.5) +
#   geom_sf(data = calibratedSensors_sf, aes(color = DIFF), size = 3) +
#   scale_color_gradientn(colors = colors_palette,  # Palette YlGnBu pour la couleur
#                         limits = c(-50, 50), 
#                         na.value = "black") +
#   theme_bw() +
#   labs(title = "Impact de la Correction sur les Données PM2.5",
#        color = "Différence (DIFF) PM2.5")
# 
# # Afficher le graphique
# plot_sens
# # Sauvegarder la carte en haute résolution avec une taille plus grande
# ggsave(file = paste0(fileOutName2, "_NoLocali2_", char_time, ".png"), plot = plot_sens, width = 10, height = 8, dpi = 300)


write.csv(
  calibrationFactorsAlltime,
  file_calibrationFactorsAlltime_csv,
  row.names = F
)
write.csv(
  calibratedSensorsAlltime,
  file_calibratedSensorsAlltime_csv,
  row.names = F
)

save(calibratedSensorsAlltime, file = file_calibratedSensorsAlltime_Rda)

