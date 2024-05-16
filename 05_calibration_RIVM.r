rm(list = ls())

# Set directory
setwd("C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/")

# Import libraries
library(raster)
library(maptools)
library(RColorBrewer)
library(fields)
library(rgdal)
library(ggplot2)
library(dplyr)
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
CRS_NT=CRS("+init=epsg:28992")

# Directory paths
indir   <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/INPUTS/"# path for input directory
outdir  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/"# path for output directory
outdir2  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/figs/"# path for output directory

# Init variables
loc <-"Netherlands" # estimation location
pol <-"PM25" # pollutant

# Init calibration
useBootstrap = T
nBootstrapSamples = 100 # should be large... Not used if useBootstrap = FALSE
referenceStationFractionAfterResampling = 0.8 # After resampling/bootstrapping
if(!useBootstrap) nBootstrapSamples = 1 # Just one calculation in case of no bootstrapping

select_time_start=as.POSIXct("2022-01-10 00:00:00", origin="1970-01-01",tz="CET") # To be changed if necessary
select_time_end=as.POSIXct("2022-01-19 00:00:00", origin="1970-01-01",tz="CET")

#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(paste0(outdir,"LCS_df_all_data_clean_groups_outliers_v2.Rda"))
sensData <- dataout
sensData <- sensData[which(sensData$datetime>=select_time_start & sensData$datetime<=select_time_end),]
sensData[which(sensData$outliers=="Outlier"),4]<-NA
sensData=sensData[complete.cases(sensData$PM2.5),]
sensData2=subset(sensData,select=-c(Typology,Clust,Season,Group,outliers))

load(paste0(indir,"ref_df_all.Rda"))
allreferenceData1=ref_df_all
allreferenceData1 <- allreferenceData1[which(allreferenceData1$datetime>=select_time_start & allreferenceData1$datetime<=select_time_end),]

station_typo <- read.csv(file=paste0(indir,"Stations_RIVM_LML_Feb2020_format_modif.csv"), header=TRUE, sep=";",skip=0)
colnames(station_typo)[1] <- "ID"
names(station_typo)[names(station_typo) == "PM2.5"] <- "PM2.5_typo"
names(station_typo)[names(station_typo) == "PM10"] <- "PM10_typo"
station_typo <- subset(station_typo, select=-c(Organisation,PM10_typo))
station_typo$PM2.5_typo[station_typo$PM2.5_typo==-999]<-NA
colnames(station_typo)[1] <- "ID"
station_typo$Representativity_mean = station_typo$Representativity_min + (station_typo$Representativity_max - station_typo$Representativity_min)/2
allreferenceData <- merge(allreferenceData1,station_typo,by="ID") # Merge ref data and typo
allreferenceData2=subset(allreferenceData,select=-c(Typology))

#####################################
#            DEFINE TIME            #          
#####################################
start_date <- min(sensData$datetime) # starting date
end_date <- max(sensData$datetime) # ending date
timeout <- seq(as.POSIXct(start_date), as.POSIXct(end_date), by="hour")
nbr_hours <- length(timeout)

#####################################
#            CALIBRATION            #          
#####################################
# Run script
source("interpolate.R")

# Loop over hours
for (ntimes in 1:nbr_hours){
  
  ndatetime=timeout[ntimes]
  
  char_time=as.character(ndatetime)
  char_time = gsub("-","",char_time);char_time = gsub(":","",char_time)
  char_time = gsub(" ","",char_time);
  char_time=substr(char_time,1,10)

  print(char_time)

  ###################
  ### Select data ###
  
  # Sensor data
  nsensData=sensData2[which(sensData2$datetime==ndatetime),]

  if (length(nsensData[,1]) > 0){

  nsensData = aggregate(. ~ ID, nsensData, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
  rownames(nsensData) = nsensData$ID
  numberOfSensors = nrow(nsensData)
  
  # Reference data 
  referenceData=allreferenceData2[which(allreferenceData2$datetime==ndatetime),]    
  referenceData = referenceData[referenceData$PM2.5>0.0,]
  referenceData = aggregate(. ~ ID, referenceData, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
  rownames(referenceData) = referenceData$ID
  numberOfReferenceStations = nrow(referenceData)
  
  #################################
  ### Caluclate distance matrix ###
  
  #calculate distance-matrix between reference stations and sensors
  # (No need to do this every hour if refererence and sensor locations are fixed).
  distancesSensorReference = data.frame()
  for(referenceInt in 1:nrow(referenceData)){
    referenceRow = referenceData[referenceInt,]
    currentRow = t(data.frame(
      sqrt((nsensData$X - referenceRow$X)^2 +
             (nsensData$Y - referenceRow$Y)^2)
    ))
    colnames(currentRow) = nsensData$ID
    rownames(currentRow) = referenceRow$ID
    distancesSensorReference = rbind(distancesSensorReference,currentRow)
  }
  
  ############################
  ### Define sensor groups ###          
  
  # Select groups of sensors around reference stations by providing:
  # 1) minCountSensor: a minimum number of sensors
  # 2) maxCountSensor: providing a maximum number of sensors
  # 3) maxTolDistSensor: providing a maximum tolerable distance between sensor and reference station
  minCountSensor = 2
  maxCountSensor = 1000
  #maxTolDistSensor = 7500
  listSensorCalibrationGroups = list()
  vectorGroupIDs = c()
  allCloseSensors = c()
  iList = 1
  
  for (iRef in 1:numberOfReferenceStations){
    #select all sensors within maxTolDistSensor from reference station. And sort by distance, starting close
    #print(paste0("Station N°",iRef))
    maxTolDistSensor = referenceData$Representativity_max[iRef]
    #print(paste0("Max distance from station is ",maxTolDistSensor,"m"))

    closeSensors =  sort(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<=maxTolDistSensor])
    nrValid = length(closeSensors)

    # if(nrValid==1){
    #   closeSensors = sort(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<=5000])
    #   closeSensors = closeSensors[1]
    # }

    #print(paste0("Number of sensors is ",nrValid))  
    referenceID = as.character(referenceData[iRef,]$ID)

    #select a maximum of maxCountSensor sensors
    if(nrValid> 0){
      closeSensors = closeSensors[1:min(length(closeSensors),maxCountSensor)]
    }
    #only create a group of sensors if more than minCountSensor sensors are available
    if (nrValid >=  minCountSensor) {
      vectorGroupIDs[iList] = referenceID
      listSensorCalibrationGroups[[iList]] = list(referenceID,
                                                  closeSensors)
      allCloseSensors = c(allCloseSensors,
                          names(closeSensors)
      )
      iList = iList + 1
    } else {
      #print(paste("Reference station ",referenceID," will not provide data for calibration") )
    }
  }
  
  # Sensors can be colocated with multiple reference locations. 
  allCloseSensors = unique(allCloseSensors)
  
  # Remove na values for current hour
  nsensData = na.omit(nsensData)
  referenceData = na.omit(referenceData)

  hourlySensorValues = as.numeric(nsensData$PM2.5)
  names(hourlySensorValues) = nsensData$ID
  
  hourlyReferenceValues  = as.numeric(referenceData$PM2.5)
  names(hourlyReferenceValues) = referenceData$ID
  
  # Use only those sensors that are part of a local group for calibration
  hourlySensorValues = hourlySensorValues[allCloseSensors]
  
  hourlySensorValues = sort(na.omit(hourlySensorValues))
  nSensors = length(hourlySensorValues)
  
  hourlySensorValuesForCalibration = hourlySensorValues
  
  ###################
  ### CALIBRATION ###          

  numberOfCalibrationGroups = length(listSensorCalibrationGroups)
  
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
        sensorIDs = names(listItem[[2]])
        
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
  
  # In case of bootstrapping, attach uncertainties
  if (nBootstrapSamples > 1 ){
    calibrationFactors = merge(calibrationFactors,
                               setNames(aggregate(. ~ ID,calibrationFactorAllSamples[,c("ID","CalibrationFactor")],
                                                  FUN  = function(x) c(quantile(x,0.05,na.rm = T),
                                                                       quantile(x,0.95,na.rm = T)),
                                                  na.action = na.pass),c("ID","Perc")),
                               by.x = "ID",by.y= "ID" , all.x = T)
    calibratedSensors = merge(calibratedSensors,
                              setNames(aggregate(. ~ ID,calibratedSensorsAllSamples[,c("ID","CalibratedPM2.5")],
                                                 FUN  = function(x) c(quantile(x,0.05,na.rm = T),
                                                                      quantile(x,0.95,na.rm = T)),
                                                 na.action = na.pass),c("ID","Perc")),
                              by.x = "ID",by.y= "ID" , all.x = T)

    fileOutName = paste0(outdir,"outliers_v2_calibrationFactors_nBootStrap_",nBootstrapSamples)
    fileOutName2 = paste0(outdir2,"outliers_v2_calibrationFactors_nBootStrap_",nBootstrapSamples)
    fileOutNameSensor = paste0(outdir,"outliers_v2_calibratedSensors_nBootStrap_",nBootstrapSamples)

  } else {

    fileOutName = paste0(outdir,"calibrationFactors")
    fileOutName2 = paste0(outdir2,"calibrationFactors")
    fileOutNameSensor = paste0(outdir,"calibratedSensors")


  }
  
  calibrationFactors$datetime=rep(ndatetime,length(calibrationFactors[,1]))	
  calibratedSensors$datetime=rep(ndatetime,length(calibratedSensors[,1]))

  # write.csv(calibrationFactors,paste0(fileOutName,"_",char_time,".csv"),row.names = F)
  # write.csv(calibratedSensors,paste0(fileOutNameSensor,"_",char_time,".csv"),row.names = F)

  # Concatenate
  if (ntimes == 1){
          calibrationFactorsAlltime=calibrationFactors
          calibratedSensorsAlltime=calibratedSensors
  }else{
          calibrationFactorsAlltime=rbind(calibrationFactorsAlltime,calibrationFactors)
          calibratedSensorsAlltime=rbind(calibratedSensorsAlltime,calibratedSensors)
  }


  # Plot results
  # library(rgdal)
  # library(ggplot2)
  # 
  # provinceNL <- readOGR("C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/shapeIn/nederlandProvinciesLines.shp")
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

write.csv(calibrationFactorsAlltime,paste0(fileOutName,"_alltime_nmax1000_distmaxRepmax_outliers.csv"),row.names = F)
write.csv(calibratedSensorsAlltime,paste0(fileOutNameSensor,"_alltime_nmax1000_distmaxRepmax_outliers.csv"),row.names = F)





