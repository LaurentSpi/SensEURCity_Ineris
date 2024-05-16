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
#                           UNCERTAINTY CALCULATION                             #
#                             Created 31/05/2022                                #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################
CRS_NT=CRS("+init=epsg:28992")

# Init variables
loc <-"Netherlands" # estimation location
pol <-"PM25" # pollutant

# Directory paths
indir   <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/INPUTS/"# path for input directory
outdir  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/"# path for output directory
outdir2  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/figs/"# path for output directory

# Data from original files sent by RIVM
file_fs_ori <- paste0(indir,"LCS_df_all.Rda")
load(file_fs_ori)
data_fs_ori_all=LCS_df_all

# Data from calibrated sensor data
file_fs <- "outliers_v2_calibratedSensors_nBootStrap_100_alltime_nmax1000_distmaxRepmax_outliers.csv"       # Fixed sensor data file
data_fs_all=read.csv(paste(outdir,file_fs,sep=""))

# Uncertainties due to calibration procedure / sensor behavior <=> confidence interval
TSU <- (data_fs_all$Perc.95.)-(data_fs_all$Perc.5.)

# Random noise / random uncertainties of the sensors (NOVA SDS011) <=> U_fs in the VME formula
random_noise = (6 + 0.13 * data_fs_all$CalibratedPM2.5)/data_fs_all$CalibratedPM2.5
data_fs_all$U_fs <- random_noise

# Save sensor data with random noise
write.csv(data_fs_all,paste0(outdir,"outliers_v2_calibratedSensors_nBootStrap_100_alltime_nmax1000_distmaxRepmax_outliers_Ufs.csv"),row.names = F)

# Reformat the dataframe for the following uncertainty calculation
data_fs_all$datetime=as.POSIXlt(data_fs_all$datetime, origin="1970-01-01",tz="CET")
data_fs_ori_cut=subset(data_fs_ori_all,select=-c(USE,TSU,PM2.5_calib,PM2.5))
data_fs_merge=merge(data_fs_all,data_fs_ori_cut,by=c("datetime","ID","X","Y"))
names(data_fs_merge)[names(data_fs_merge) == "PM2.5"] <- "PM2.5_synth"
names(data_fs_merge)[names(data_fs_merge) == "CalibratedPM2.5"] <- "PM2.5_cal"
df=data_fs_merge
df=subset(df,select=-c(Perc.5.,Perc.95.))
df <- df[, c(1,2,3,4,5,6,8,7)]

###########################################
#       	Uncertainty calculation	    	  #	
# from L. Spinelle (INERIS) methodology   #
###########################################

###############
## Functions ##
###############

## Between sensor uncertainty ##

uBss <- function(DataFrame, Sensors, DateInit, DateEnd){
  ## This function return the uncertainty between sensor systems as described in the darft of the sensor evaluation protocol fprm the CEN-TC264-WG42.
  ## This is to be considered as the reproducibility between the different replicas of the evaluated systems.
  ## The value is given in the same unit as the sensor measurement (e.g. ppb, ug/m3...).
  ## DataFrame = general dataframe containing the dataset.
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

## Field uncertainty ##

UCi <- function(DataFrame, Reference, Sensor, CalModel, ubRM, LV, Corr = NA){
  ## This function return the uncertainty of measurements of the field tests as described in the darft of the sensor evaluation protocol fprm the CEN-TC264-WG42.
  ## The value is given in %.
  ## DataFrame = general dataframe containing the dataset
  ## Reference = reference column
  ## Sensor = sensor column
  ## CalModel = intercept (a) and slope (b) of the linear correction Y = a+bX as c(a,b)
  ## ubRM = value of the between refence method uncertainty
  ## LV = limit value af the corresponding pollutant
  ## Corr = if needed, type of correction used for the uncertainty evaluation as described within the CEN/TC264/WG42 document
  ##        4 possibilities:
  ##          - NA = default value, imply no correction post evaluation in the dataset
  ##          - "intercept" = if only the intercept correction is needed post evaluation
  ##          - "slope" = if only the slope correction is needed post evaluation
  ##          - "linear" = if both slope and intercept corrections are needed post evaluation
  
  #UCi.df <- DataFrame[, c(Reference,Sensor)]
  UCi.df <- subset(DataFrame, select=c(Reference,Sensor))
  
  UCi.df <- replace(UCi.df, is.na(UCi.df), NA)
  RSS_CalModel <- as.data.frame((UCi.df[, Sensor]-CalModel[1]-CalModel[2]*UCi.df[, Reference])^2)
  colnames(RSS_CalModel) <- "RSS"
  UCi.df <- cbind(UCi.df, RSS_CalModel)
  RSS <- sum(UCi.df$RSS, na.rm = TRUE)
  Sxx    <- sum((UCi.df[, Reference] - mean(UCi.df[, Reference], na.rm = TRUE))^2, na.rm = TRUE)
  Syy    <- sum((UCi.df[, Sensor] -  mean(UCi.df[, Sensor], na.rm = TRUE))^2, na.rm = TRUE)
  Sxy    <- sum((UCi.df[, Reference] - mean(UCi.df[, Reference], na.rm = TRUE)) * (UCi.df[, Sensor] -  mean(UCi.df[, Sensor], na.rm = TRUE)), na.rm = TRUE)
  ub    <- sqrt((Syy - (Sxy^2/Sxx))/((nrow(UCi.df)-2)*Sxx))
  ua    <- sqrt(ub^2 * sum(UCi.df[, Reference]^2, na.rm = TRUE)/nrow(UCi.df))
  
  if (is.na(Corr)) {
    UCi <- 2*sqrt(((RSS/(nrow(UCi.df)-2))-ubRM^2)+(CalModel[1]+(CalModel[2]-1)*LV)^2)
  }
  if (isTRUE(Corr == "intercept")) {
    UCi <- 2*sqrt(((RSS/(nrow(UCi.df)-2))-ubRM^2)+(CalModel[1]+(CalModel[2]-1)*LV)^2+ua^2)
  }
  if (isTRUE(Corr == "slope")) {
    UCi <- 2*sqrt(((RSS/(nrow(UCi.df)-2))-ubRM^2)+(CalModel[1]+(CalModel[2]-1)*LV)^2+(LV^2*ub^2))
  }
  if (isTRUE(Corr == "linear")) {
    UCi <- 2*sqrt(((RSS/(nrow(UCi.df)-2))-ubRM^2)+(CalModel[1]+(CalModel[2]-1)*LV)^2+ua^2+(LV^2*ub^2))
  }
  return(UCi)
}

## Example of ubRM (uncertainty of reproducibility of the reference method, in Âµg/m3) and LV (limit values).
## Define LV and ubRM for NO2
LV_NO2 <- 200/1.88
ubRM_NO2 <- 0.03*LV_NO2
## Define LV and ubRM for O3
LV_O3 <- 60
ubRM_O3 <- 0.03*LV_O3
## Define LV and ubRM for PM1
LV_PM1 <- 50
ubRM_PM1 <- 0.03*LV_PM1
## Define LV and ubRM for PM2.5
LV_PM25 <- 50
ubRM_PM25 <- 0.03*LV_PM25
## Define LV and ubRM for PM10
LV_PM10 <- 50
ubRM_PM10 <- 0.03*LV_PM10


## According to the GDE / using real value as the reference in the uncertainty caluculation
ubRM=0.67

###########################
##    Uncertainty calc   ##
###########################
start_date=min(data_fs_all$datetime)
end_date=max(data_fs_all$datetime)

sensors = unique(data_fs_all$ID)
nbr_sensors=length(sensors)

df_all=c()

for (nsens in 1:nbr_sensors){
	isens=sensors[nsens]
	idf=df[df$ID==isens,]
	idf=subset(idf,select=c(PM2.5_cal,PM2.5_synth,datetime))
	names(idf)[1] = isens
        names(idf)[2] = paste0(isens,"_REF")
	if (nsens > 1 ){
		df_all=merge(df_all,idf,by=c("datetime"),all=TRUE)
	}else{
		df_all=idf	
	}
}

#tmp=subset(df,select=c(datetime,PM2.5_real))
#df_all_final=merge(df_all,by="datetime",tmp,all=TRUE)
#df_all_final=df_all

# Between sensor uncertainty
PM25_uBss = uBss(df_all,sensors,start_date,end_date)

# Field uncertainty

df2 <- df_all

UCi_all <- c()

for (nsens in 1:nbr_sensors){

	isens1 = sensors[nsens]
	isens1_ref = paste0(isens1,"_REF")
	tst=grepl("-", isens1)
	
	if (tst==TRUE){
		isens=gsub("-","_",isens1)
		names(df2)[names(df2) == isens1] <- isens
		isens_ref=gsub("-","_",isens1_ref)
		names(df2)[names(df2) == isens1_ref] <- isens_ref
	}else{
		isens=isens1
		isens_ref=isens1_ref
	}

	formula=as.formula(paste0(isens_ref,"~",isens))
	lm1 <- lm(formula, data=df2)
	calModel=c(lm1$coefficients[[1]],lm1$coefficients[[2]])
	UCi_isens <- UCi(df2, isens_ref, isens, calModel, ubRM, LV_PM25, Corr = NA)
	UCi_tmp <- cbind(isens1,UCi_isens)
	UCi_all <- rbind(UCi_all,UCi_tmp)

}

UCi_all = as.data.frame(UCi_all)
UCi_all$UCi_isens=as.numeric(UCi_all$UCi_isens)
names(UCi_all)[1] <- "ID"
names(UCi_all)[2] <- "UCi"
UCi_all$UCi=UCi_all$UCi/LV_PM25

dataout = merge(data_fs_all,UCi_all,by="ID")

write.csv(dataout,paste0(outdir,"outliers_v2_calibratedSensors_nBootStrap_100_alltime_nmax1000_distmaxRepmax_outliers_Ufs_UCi.csv"), row.names = F)





















