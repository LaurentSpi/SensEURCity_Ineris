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
library(tidyr)
require(sp)
require(rgdal)

#################################################################################
#                          SENSOR DATA CLASSIFICATION                           #
#                  Created 22/06/2021 updated on 25/08/2021                     #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################

# Define projections
CRS_L93=CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
CRS_NT=CRS("+init=epsg:28992")

# Directory paths
indir   <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/INPUTS/"# path for input directory
outdir  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/"# path for output directory
outdir2  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/figs/"# path for output directory

# Init variables
loc <-"Netherlands" # estimation location
pol <-"PM25" # pollutant

#####################################
#           LOAD SIG DATA           #          
#####################################
print("READ SIG DATA")

# Coast lines
gadm <- readRDS(paste0(indir,"gadm36_NLD_1_sp.rds"))
gadm <- spTransform(gadm,CRS_NT)

# Corine Land Cover
CLC_Netherlands_NT <- raster(paste0(indir,"CLC_Netherlands_NT.tif"))
legend_CLC <- read.csv(file=paste0(indir,"legend_CLC_V4.csv"), header=TRUE, sep=";",skip=0)

# Road network
load(paste0(indir,"road_net2.Rdata"))

# Population density
pop_NT <- raster(paste0(indir,"pop_NT.tif"))

#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(paste0(outdir,"LCS_df_all_clean.Rda"))
load(paste0(indir,"ref_df_all.Rda"))

### Reference data ###
ref_df_all=ref_df_all[ref_df_all$PM2.5 > 0,] # Only keep positive values
ID_ref=unique(ref_df_all$ID)

# Typology
station_typo <- read.csv(file=paste0(indir,"Stations_RIVM_LML_Feb2020_format_modif.csv"), header=TRUE, sep=";",skip=0)
colnames(station_typo)[1] <- "ID"
names(station_typo)[names(station_typo) == "PM2.5"] <- "PM2.5_typo"
names(station_typo)[names(station_typo) == "PM10"] <- "PM10_typo"
station_typo <- subset(station_typo, select=-c(Organisation,PM10_typo))
station_typo$PM2.5_typo[station_typo$PM2.5_typo==-999]<-NA
station_typo$Typology <- gsub('Industrial setting', 'Industrial Setting', station_typo$Typology)

stadata <- merge(ref_df_all,station_typo,by="ID") # Merde ref data and typo
stadata=subset(stadata,select=-c(Representativity_min,PM2.5_typo))

stadata2 = subset(stadata, select=-c(datetime,Typology))
stadata2$ID=as.character(stadata2$ID)
stadata2 = aggregate(. ~ ID, stadata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(stadata2) = stadata2$ID
nbr_sta = nrow(stadata2)

### Sensor data ###
sensdata <- dataout
sensdata2 = subset(sensdata, select=-c(datetime))
sensdata2$ID=as.character(sensdata2$ID)
sensdata2 = aggregate(. ~ ID, sensdata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(sensdata2) = sensdata2$ID
nbr_sensor = nrow(sensdata2)

# Define start and end date
start_date <- min(stadata$datetime) # starting date
end_date <-max(stadata$datetime) # ending date

#####################################
#         ASSIGN TYPOLOGY           #          
#####################################

print("START ASSIGN TYPOLOGY")

## 1) CLC
# Consider data only within Netherlands
Xmin=13895.64; Xmax=277998.5; Ymin=303925.3;Ymax=619270.2 # define x and y max/min based on coast lines
e<-extent(Xmin,Xmax,Ymin,Ymax)
CLC_Netherlands_NT_crop=crop(CLC_Netherlands_NT,e)
sensdata2_crop=sensdata2[which(sensdata2$X <= Xmax & sensdata2$X >= Xmin & sensdata2$Y >= Ymin & sensdata2$Y <= Ymax),] # crop sensor data

# Convert df to spatial object
spsensdata2=sensdata2_crop # crop sensor data
spsensdata2$Long <- spsensdata2$X; spsensdata2$Lat <- spsensdata2$Y
coordinates(spsensdata2)=~Long+Lat
proj4string(spsensdata2)=CRS_NT
tmp = spsensdata2@coords; dlon=tmp[,1]; dlat=tmp[,2]
spsensdata2$lon <- dlon; spsensdata2$lat <- dlat
class(spsensdata2); summary(spsensdata2)

# Extract raster value at sensor location based on a buffer of 1m
CLC_sens <- raster::extract(CLC_Netherlands_NT_crop,             # raster layer
    spsensdata2,   # SPDF with centroids for buffer
    buffer = 1,    # buffer size, units depend on CRS
    fun=median,  # what to value to extract
    na.rrm=TRUE,   # rm NAs
    df=TRUE)       # return a dataframe? 

CLC_sens$CLC_Netherlands_NT[CLC_sens$CLC_Netherlands_NT==128]<-NA

sensdata2_crop$CLC <- CLC_sens$CLC_Netherlands_NT

CLC_val <- legend_CLC
CLC_val=subset(CLC_val,select=-c(v2,v3,v4,v5,v6))
colnames(CLC_val)[1] <- "CLC"
names(CLC_val)[names(CLC_val) == "type"] <- "LandType"
names(CLC_val)[names(CLC_val) == "type2"] <- "Typology"

sensdata3=merge(sensdata2_crop,CLC_val,by="CLC")

# Convert df to spatial object
spsensdata3=sensdata3 # sensor data with typology
spsensdata3$Long <- spsensdata3$X; spsensdata3$Lat <- spsensdata3$Y
coordinates(spsensdata3)=~Long+Lat
proj4string(spsensdata3)=CRS_NT
tmp = spsensdata3@coords; dlon=tmp[,1]; dlat=tmp[,2]
spsensdata3$lon <- dlon; spsensdata3$lat <- dlat
class(spsensdata3); summary(spsensdata3)

sensdata_T<-spsensdata3[which(spsensdata3$Typology=="Traffic"),]
sensdata_I<-spsensdata3[which(spsensdata3$Typology=="Industrial Setting"),]
sensdata_UB<-spsensdata3[which(spsensdata3$Typology=="Urban background"),]
sensdata_RB<-spsensdata3[which(spsensdata3$Typology=="Regional background"),]

# Plot
png(paste(outdir2, "03_",loc,"_Sensordata_Typo.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(sensdata_UB,pch=21,col='blue',bg="blue",add=T,cex=1)
plot(sensdata_RB,pch=21,col='green',bg="green",add=T,cex=1)
plot(sensdata_T,pch=21,col='red',bg="red",add=T,cex=1)
plot(sensdata_I,pch=21,col='orange',bg="orange",add=T,cex=1)
legend("topleft",legend=c("Urban background", "Regional background","Traffic","Industrial setting"),col=c("blue","green","red","orange"),pch=c(16,16,16,16),cex=2)
dev.off()

## 2) POP

# Commuting zone
pop2_NT <- pop_NT
values(pop2_NT)[values(pop2_NT) < 2.5] = NA

# Rural areas
pop3_NT <- pop_NT
values(pop3_NT)[values(pop3_NT) < 3.5] = NA

zl <- c(0,16)
pop_NT[which(pop_NT@data@values < zl[1])] <- zl[1]
pop_NT[which(pop_NT@data@values > zl[2])] <- zl[2]
pop2_NT[which(pop2_NT@data@values < zl[1])] <- zl[1]
pop2_NT[which(pop2_NT@data@values > zl[2])] <- zl[2]
pop3_NT[which(pop3_NT@data@values < zl[1])] <- zl[1]
pop3_NT[which(pop3_NT@data@values > zl[2])] <- zl[2]

palette<-colorRampPalette(c("slateblue4","slateblue2","yellow","red2","red4"))(128)
mycol=palette
ncol=100
brks <- seq(zl[1], zl[2], length.out = ncol+1)
brkslab <- format(brks, scientific=FALSE, digits=2)
indbrks <-  seq(1,length(brks), by = 15)
mycol_ncol <- colorRampPalette(mycol)(ncol)

# Plot pop all
# png(paste(outdir2, loc,"_pop.png",sep=""),width=900, height=900)
# plot(gadm, border="black")
# plot(pop_NT,add=T,scale=1, col=mycol_ncol, zlim=zl,axis.args = list(at=brks[indbrks], labels=brkslab[indbrks], cex.axis=1.2))
# dev.off()
# 
# # Plot pop commuting zones
# png(paste(outdir2, loc,"_pop_commuting_zones.png",sep=""),width=900, height=900)
# plot(gadm, border="black")
# plot(pop2_NT,add=T,scale=1, col=mycol_ncol, zlim=zl,axis.args = list(at=brks[indbrks], labels=brkslab[indbrks], cex.axis=1.2))
# dev.off()
# 
# # Plot pop cities
# png(paste(outdir2, loc,"_pop_cities.png",sep=""),width=900, height=900)
# plot(gadm, border="black")
# plot(pop3_NT,add=T,scale=1, col=mycol_ncol, zlim=zl,axis.args = list(at=brks[indbrks], labels=brkslab[indbrks], cex.axis=1.2))
# dev.off()

# Extract raster value at sensor location based on a buffer of 0m = exact location
POP_sens <- raster::extract(pop_NT,             # raster layer
    spsensdata3,   # SPDF with centroids for buffer
    buffer = 0,    # buffer size, units depend on CRS
    fun=median,    # what to value to extract
    na.rrm=TRUE,   # rm NAs
    df=TRUE)       # return a dataframe? 

sensdata3$POP <- POP_sens$pop_NT
sensdata3=sensdata3[,c(2,3,4,5,1,6,7,8)]
sensdata4=sensdata3
sensdata4[which(sensdata4$POP>=3.5),7]<-"Urban background"
sensdata4[which(sensdata4$POP<=2.5),7]<-"Regional background"

# Convert df to spatial object
spsensdata4=sensdata4 # sensor data with typology
spsensdata4$Long <- spsensdata4$X; spsensdata4$Lat <- spsensdata4$Y
coordinates(spsensdata4)=~Long+Lat
proj4string(spsensdata4)=CRS_NT
tmp = spsensdata4@coords; dlon=tmp[,1]; dlat=tmp[,2]
spsensdata4$lon <- dlon; spsensdata4$lat <- dlat
class(spsensdata4); summary(spsensdata4)

sensdata_T<-spsensdata4[which(spsensdata4$Typology=="Traffic"),]
sensdata_I<-spsensdata4[which(spsensdata4$Typology=="Industrial Setting"),]
sensdata_UB<-spsensdata4[which(spsensdata4$Typology=="Urban background"),]
sensdata_RB<-spsensdata4[which(spsensdata4$Typology=="Regional background"),]

# Plot
png(paste(outdir2, "03_",loc,"_Sensordata_Typo_corrbyPOP.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(sensdata_UB,pch=21,col='blue',bg="blue",add=T,cex=1)
plot(sensdata_RB,pch=21,col='green',bg="green",add=T,cex=1)
plot(sensdata_T,pch=21,col='red',bg="red",add=T,cex=1)
plot(sensdata_I,pch=21,col='orange',bg="orange",add=T,cex=1)
legend("topleft",legend=c("Urban background", "Regional background","Traffic","Industrial setting"),col=c("blue","green","red","orange"),pch=c(16,16,16,16),cex=2)
dev.off()

## 3) ROADS NET

road_pts = as(road_net2, "SpatialPointsDataFrame")
road_pts2=subset(road_pts,select=c(GP_RTP))

buffer=5 #in meters
prox_roads <- rep(0, length(spsensdata4))

for (npts in 1:length(spsensdata4)){
    spdata_tmp = spsensdata4[npts,]
    dist_vector = spDistsN1(road_pts2,spdata_tmp)
    #dist_vector = dist_vector * 1e3 # km to m
    dist_min=min(dist_vector)

    if (dist_min <= buffer){
        prox_roads[npts]=TRUE
    }else{
        prox_roads[npts]=FALSE
    }
}

spsensdata4$prox_roads=prox_roads
spsensdata5<-spsensdata4
spsensdata5[which(spsensdata5$prox_roads==TRUE),7]<-"Traffic"

sensdata_T<-spsensdata5[which(spsensdata5$Typology=="Traffic"),]
sensdata_I<-spsensdata5[which(spsensdata5$Typology=="Industrial Setting"),]
sensdata_UB<-spsensdata5[which(spsensdata5$Typology=="Urban background"),]
sensdata_RB<-spsensdata5[which(spsensdata5$Typology=="Regional background"),]

png(paste(outdir2, "03_",loc,"_Sensordata_Typo_corrbyPOP_ROADSprox5m.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(sensdata_UB,pch=21,col='blue',bg="blue",add=T,cex=1)
plot(sensdata_RB,pch=21,col='green',bg="green",add=T,cex=1)
plot(sensdata_T,pch=21,col='red',bg="red",add=T,cex=1)
plot(sensdata_I,pch=21,col='orange',bg="orange",add=T,cex=1)
legend("topleft",legend=c("Urban background", "Regional background","Traffic","Industrial setting"),col=c("blue","green","red","orange"),pch=c(16,16,16,16),cex=2)
dev.off()

### Comparison with reference station typo

# Add stations with their typo to compare
statypo=subset(station_typo,select=c(ID,Typology))
statypo$ID=as.character(statypo$ID)

stadata3 <- merge(stadata2,statypo,by="ID") # Merge ref data and typo

spstadata <- stadata3
spstadata$Long <- spstadata$X; spstadata$Lat <- spstadata$Y
coordinates(spstadata)=~Long+Lat
proj4string(spstadata)=CRS_NT
tmp = spstadata@coords; dlon=tmp[,1]; dlat=tmp[,2]
spstadata$lon <- dlon; spstadata$lat <- dlat
class(spstadata); summary(spstadata)

stadata_T<-spstadata[which(spstadata$Typology=="Traffic"),]
stadata_I<-spstadata[which(spstadata$Typology=="Industrial Setting"),]
stadata_UB<-spstadata[which(spstadata$Typology=="Urban background"),]
stadata_RB<-spstadata[which(spstadata$Typology=="Regional background"),]

png(paste(outdir2, "03_",loc,"_Sensordata_Typo_corrbyPOP_ROADSprox5m_Refdata_Typo.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(sensdata_UB,pch=21,col='blue',bg="blue",add=T,cex=1)
plot(sensdata_RB,pch=21,col='green',bg="green",add=T,cex=1)
plot(sensdata_T,pch=21,col='red',bg="red",add=T,cex=1)
plot(sensdata_I,pch=21,col='orange',bg="orange",add=T,cex=1)

plot(stadata_UB,pch=24,col='black',bg="blue",add=T,cex=2)
plot(stadata_RB,pch=24,col='black',bg="green",add=T,cex=2)
plot(stadata_T,pch=24,col='black',bg="red",add=T,cex=2)
plot(stadata_I,pch=24,col='black',bg="orange",add=T,cex=2)

legend("topleft",legend=c("Urban background", "Regional background","Traffic","Industrial setting"),col=c("blue","green","red","orange"),pch=c(16,16,16,16),cex=2)
dev.off()

sensdata5=as.data.frame(spsensdata5)
senstypo=subset(sensdata5,select=c(ID,Typology))
sensdata_wtypo=merge(sensdata2,senstypo,by="ID")

# Check typology assignment / reference station

#Calculate distance-matrix between reference stations and sensors
distancesSensorReference = data.frame()
for(referenceInt in 1:nrow(stadata2)){
  referenceRow = stadata2[referenceInt,]
  currentRow = t(data.frame(
    sqrt((sensdata2$X - referenceRow$X)^2 +
           (sensdata2$Y - referenceRow$Y)^2)
  ))
  colnames(currentRow) = sensdata2$ID
  rownames(currentRow) = referenceRow$ID
  distancesSensorReference = rbind(distancesSensorReference,currentRow)
}

# Select groups of sensors around reference stations by providing:
# 1) minCountSensor: a minimum number of sensors
# 2) maxCountSensor: providing a maximum number of sensors
# 3) maxTolDistSensor: providing a maximum tolerable distance between sensor and reference station that depends on the station typology = representativeness
minCountSensor = 1
maxCountSensor = 1000

listSensorCalibrationGroups = list()
vectorGroupIDs = c()
allCloseSensors = c()
iList = 1

NotSelectedStadata<-c()

for (iRef in 1:nbr_sta){
  #select all sensors within maxTolDistSensor from reference station. And sort by distance, starting close
  maxTolDistSensor = stadata2$Representativity_max[iRef]
  closeSensors =  sort(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<=maxTolDistSensor])
  nrValid = length(closeSensors)
  if(nrValid==1){
      closeSensors = sort(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<=5000])
      closeSensors = closeSensors[1]
  }
  referenceID = as.character(stadata2[iRef,]$ID)
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

    NotSelectedStadata_tmp <- stadata2[which(stadata2$ID==referenceID),]
    NotSelectedStadata <- rbind(NotSelectedStadata,NotSelectedStadata_tmp)


  }
}

#Sensors can be colocated with multiple reference locations. 
allCloseSensors = unique(allCloseSensors)

#Look at the selected/notselected sensors and reference data as SpatialDataPoints
SelectedSensdata<-c()
for (nsens in 1:length(allCloseSensors)){
    SelectedSensdata_tmp=sensdata2[which(sensdata2$ID==allCloseSensors[nsens]),]
    SelectedSensdata=rbind(SelectedSensdata,SelectedSensdata_tmp)
}

SelectedSensdata$Long <- SelectedSensdata$X; SelectedSensdata$Lat <- SelectedSensdata$Y
coordinates(SelectedSensdata)=~Long+Lat
proj4string(SelectedSensdata)=CRS_NT
tmp = SelectedSensdata@coords; dlon=tmp[,1]; dlat=tmp[,2]
SelectedSensdata$lon <- dlon; SelectedSensdata$lat <- dlat
class(SelectedSensdata); summary(SelectedSensdata)

NotSelectedSensdata=sensdata2[!(sensdata2$ID %in% SelectedSensdata$ID),]
NotSelectedSensdata$Long <- NotSelectedSensdata$X; NotSelectedSensdata$Lat <- NotSelectedSensdata$Y
coordinates(NotSelectedSensdata)=~Long+Lat
proj4string(NotSelectedSensdata)=CRS_NT
tmp = NotSelectedSensdata@coords; dlon=tmp[,1]; dlat=tmp[,2]
NotSelectedSensdata$lon <- dlon; NotSelectedSensdata$lat <- dlat
class(NotSelectedSensdata); summary(NotSelectedSensdata)

SelectedStadata <- stadata2[!(stadata2$ID %in% NotSelectedStadata$ID),]
SelectedStadata$Long <- SelectedStadata$X; SelectedStadata$Lat <- SelectedStadata$Y
coordinates(SelectedStadata)=~Long+Lat
proj4string(SelectedStadata)=CRS_NT
tmp = SelectedStadata@coords; dlon=tmp[,1]; dlat=tmp[,2]
SelectedStadata$lon <- dlon; SelectedStadata$lat <- dlat
class(SelectedStadata); summary(SelectedStadata)

NotSelectedStadata$Long <- NotSelectedStadata$X; NotSelectedStadata$Lat <- NotSelectedStadata$Y
coordinates(NotSelectedStadata)=~Long+Lat
proj4string(NotSelectedStadata)=CRS_NT
tmp = NotSelectedStadata@coords; dlon=tmp[,1]; dlat=tmp[,2]
NotSelectedStadata$lon <- dlon; NotSelectedStadata$lat <- dlat
class(NotSelectedStadata); summary(NotSelectedStadata)

#Do plot
png(paste(outdir2, "03_",loc,"_sensors_station_vicinity.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(SelectedSensdata,pch=21,col='springgreen3',bg="springgreen3",add=T,cex=1)
plot(NotSelectedSensdata,pch=21,col='black',bg="black",add=T,cex=1)
plot(SelectedStadata,pch=24,col='red',bg="red",add=T,cex=2)
plot(NotSelectedStadata,pch=24,col="gray48",bg="gray48",add=T,cex=2)
legend("topleft",legend=c("Selected sensors","Not selected sensors","Selected ref stations", "Not selected ref stations"),col=c("springgreen3","black","red","gray48"),pch=c(16,16,17,17),cex=2)
dev.off()

# Do comparison
sensdata_wtypo_corr <- sensdata_wtypo

# dataout_final <- c()
# 
# for(listItem in listSensorCalibrationGroups){
# 
#   referenceID = listItem[[1]][1]
#   referenceSet = stadata3
#   referenceSubSet = referenceSet[which(referenceSet$ID==referenceID),]
# 
#   # Select sensor ID's in Group
#   df_sensors = as.data.frame(listItem[[2]])
#   sensorIDs = colnames(df_sensors)
#   sensdataSubSet<-c()
#   for (nsensor in 1:length(sensorIDs)){
#        sensdataSubSetTmp = sensdata_wtypo[which(sensdata_wtypo$ID==sensorIDs[nsensor]),]
#        sensdataSubSetTmp$Dist = rep(df_sensors[,nsensor],length(sensdataSubSetTmp[,1]))
#        sensdataSubSet = rbind(sensdataSubSet,sensdataSubSetTmp)
#    }
# 
# 
#    # Stat
#    repdist=unique(referenceSubSet$Representativity_min)
#    reftypo=as.character(referenceSubSet$Typology)
#    senstypo_all=as.character(sensdataSubSet$Typology)
#    senstypo_UT <- which(senstypo_all=="Urban background")
#    senstypo_TT <- which(senstypo_all=="Traffic")
#    senstypo_RT <- which(senstypo_all=="Regional background")
#    senstypo_IT <- which(senstypo_all=="Industrial Setting")
#    sametypo=which(senstypo_all==reftypo)
#    sametypo_perc=round((length(sametypo)/length(senstypo_all))*100)
#    Urban <-round((length(senstypo_UT)/length(senstypo_all))*100)
#    Traffic<-round((length(senstypo_TT)/length(senstypo_all))*100)
#    Regional<-round((length(senstypo_RT)/length(senstypo_all))*100)
#    Industrial<-round((length(senstypo_IT)/length(senstypo_all))*100)
#    dataout <- cbind(referenceID,reftypo,repdist,nsensor,sametypo_perc,Urban,Traffic,Regional,Industrial)
#    dataout_final <- rbind(dataout_final,dataout)
# 
#    # Typo correction based on station typo
#    sensdata_wtypo_corr[which(sensdata_wtypo_corr$ID %in% sensorIDs),5]<-reftypo
# 
# }
# 
# dataout_final=as.data.frame(dataout_final)
# dataout_final$sametypo_perc=as.numeric(as.character(dataout_final$sametypo_perc))
# write.table(dataout_final, file =paste0(outdir, "typo_verif.csv"), append = FALSE, quote = TRUE, sep = ";")

# Assign typo to sensors measurements for classification
senstypo=subset(sensdata_wtypo_corr,select=c(ID,Typology))
sensdataf=merge(sensdata,senstypo,by="ID")

# Plot
spsensdataf <- sensdataf
spsensdataf$Long <- spsensdataf$X; spsensdataf$Lat <- spsensdataf$Y
coordinates(spsensdataf)=~Long+Lat
proj4string(spsensdataf)=CRS_NT
tmp = spsensdataf@coords; dlon=tmp[,1]; dlat=tmp[,2]
spsensdataf$lon <- dlon; spsensdataf$lat <- dlat
class(spsensdataf); summary(spsensdataf)

# sensdataf_T<-spsensdataf[which(spsensdataf$Typology=="Traffic"),]
# sensdataf_I<-spsensdataf[which(spsensdataf$Typology=="Industrial Setting"),]
# sensdataf_UB<-spsensdataf[which(spsensdataf$Typology=="Urban background"),]
# sensdataf_RB<-spsensdataf[which(spsensdataf$Typology=="Regional background"),]
# 
# png(paste(outdir2, "03_",loc,"_Sensordata_Typo_corrbyPOP_ROADSprox5m_corrbyRefdata_Typo.png",sep=""),width=900, height=900)
# plot(gadm, border="black")
# plot(sensdataf_UB,pch=21,col='blue',bg="blue",add=T,cex=1)
# plot(sensdataf_RB,pch=21,col='green',bg="green",add=T,cex=1)
# plot(sensdataf_T,pch=21,col='red',bg="red",add=T,cex=1)
# plot(sensdataf_I,pch=21,col='orange',bg="orange",add=T,cex=1)
# 
# legend("topleft",legend=c("Urban background", "Regional background","Traffic","Industrial setting"),col=c("blue","green","red","orange"),pch=c(16,16,16,16),cex=2)
# dev.off()

# Save sensor data with typo
save(sensdataf,file=paste0(outdir,"LCS_df_all_clean_typo.Rda"))
save(senstypo,file=paste0(outdir,"LCS_typo.Rda"))

######################################
#            CLUSTER                 #          
######################################
spsensdata2clust <- sensdata_wtypo_corr
spsensdata2clust$Long <- spsensdata2clust$X; spsensdata2clust$Lat <- spsensdata2clust$Y
coordinates(spsensdata2clust)=~Long+Lat
proj4string(spsensdata2clust)=CRS_NT
tmp = spsensdata2clust@coords; dlon=tmp[,1]; dlat=tmp[,2]
spsensdata2clust$lon <- dlon; spsensdata2clust$lat <- dlat
class(spsensdata2clust); summary(spsensdata2clust)

# Look for clusters
chc <- hclust(dist(data.frame(rownames=rownames(spsensdata2clust@data), x=coordinates(spsensdata2clust)[,1],
              y=coordinates(spsensdata2clust)[,2])), method="complete")

chc.d10000 <- cutree(chc, h=10000) 
spsensdata2clust@data <- data.frame(spsensdata2clust@data, Clust=chc.d10000)

nb.cols <- max(spsensdata2clust@data$Clust)
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

png(paste(outdir2, "03_",loc,"_Sensordata_Cluster.png",sep=""),width=900, height=900)
plot(gadm, border="black")
plot(spsensdata2clust,col=mycolors, pch=19,add=T)
dev.off()

df_clusters=as.data.frame(spsensdata2clust)

sensdata_wtypo_corr_cluster = as.data.frame(spsensdata2clust)
sensdata_wtypo_corr_cluster = subset(sensdata_wtypo_corr_cluster,select=-c(X,Y,PM2.5,Typology,Long,Lat,lon,lat))

sensdata_wtypo_corr_cluster2=sensdata_wtypo_corr_cluster
sensdata_wtypo_corr_cluster2 = subset(sensdata_wtypo_corr_cluster2,select=-c(ID))
sensdata_wtypo_corr_cluster2$Count=rep(1,length(sensdata_wtypo_corr_cluster2[,1]))
stat_clust = aggregate(. ~ Clust, sensdata_wtypo_corr_cluster2, function(x) sum(x, na.rm=TRUE), na.action = na.pass)
stat_clust = subset(stat_clust,select=c(Clust,Count))
clust2out = which(stat_clust$Count > 10)

sensdataf2=merge(sensdataf,sensdata_wtypo_corr_cluster,by=c("ID"))

######################################
#          ASSIGN SEASON             #          
######################################

# Winter (DJF)
winter_start=as.POSIXlt("2022-01-01 00:00:00", origin="1970-01-01",tz="CET")
winter_end=as.POSIXlt("2022-02-28 23:59:59", origin="1970-01-01",tz="CET")
# Spring (MAM)
#spring_start=as.POSIXlt("2021-03-01 00:00:00", origin="1970-01-01",tz="CET")
#spring_end=as.POSIXlt("2021-05-31 23:59:59", origin="1970-01-01",tz="CET")
# Summer (JJA)
#summer_start=as.POSIXlt("2021-06-01 00:00:00", origin="1970-01-01",tz="CET")
#summer_end=as.POSIXlt("2021-08-31 23:59:59", origin="1970-01-01",tz="CET")
# Fall (SON)
#fall_start=as.POSIXlt("2021-09-01 00:00:00", origin="1970-01-01",tz="CET")
#fall_end=as.POSIXlt("2021-11-30 23:59:59", origin="1970-01-01",tz="CET")

Season <- rep("no season",length(sensdataf2[,1]))
sensdataff<-sensdataf2
sensdataff$Season=Season

sensdataff[which(sensdataff$datetime >= winter_start & sensdataff$datetime <= winter_end),8]<-"Winter"
#sensdataff[which(sensdataff$datetime >= spring_start & sensdataff$datetime <= spring_end),8]<-"Spring"
#sensdataff[which(sensdataff$datetime >= summer_start & sensdataff$datetime <= summer_end),8]<-"Summer"
#sensdataff[which(sensdataff$datetime >= fall_start & sensdataff$datetime <= fall_end),8]<-"Fall"

######################################
#     ASSIGN WEEK DAYS/ WEEKENDS     #          
######################################

Weekdays <- rep("weekdays",length(sensdataff[,1]))
sensdataff$Weekdays=Weekdays
sensdataff$Weekdays=weekdays(as.POSIXct(sensdataff$datetime), abbreviate = F)

DayType <- rep("Weekday",length(sensdataff[,1]))
sensdataff$DayType=DayType

sensdataff[which(sensdataff$Weekdays == "Saturday" | sensdataff$Weekdays == "Sunday"),10]<-"Weekend"

######################################
#           ASSIGN PERIODS           #          
######################################

sensdataff$Hour=as.numeric(format(sensdataff$datetime, format = "%H"))
Periods <- rep("periods",length(sensdataff[,1]))
sensdataff$Periods=Periods

sensdataff[which(sensdataff$Hour > 6 & sensdataff$Hour <= 9 | sensdataff$Hour > 16 & sensdataff$Hour <= 20),12]<-"Traffic hours"
sensdataff[which(sensdataff$Hour > 9 & sensdataff$Hour <= 16 | sensdataff$Hour > 20 & sensdataff$Hour <= 22),12]<-"Off-peak hours"
sensdataff[which(sensdataff$Hour > 22 | sensdataff$Hour == 0 | sensdataff$Hour > 5 & sensdataff$Hour <= 6),12]<-"Transition periods"
sensdataff[which(sensdataff$Hour >= 1 & sensdataff$Hour <= 5),12]<-"Night hours"

sensdataff=subset(sensdataff,select=-c(Hour,Weekdays))

# Plot
source("multiplot.r")
png(paste(outdir2,"03_",loc,"_sensors_boxplots_typo_season_day_hour.png",sep=""),width=800, height=700)
P1 <- ggplot(sensdataff,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=Season)) + ylim(0, 30) +
      labs(y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'))
P1

P2 <- ggplot(sensdataff,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=DayType)) + ylim(0, 30) +
      labs(y=bquote(PM2.5 ~ (mu*g/m^3)))+
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'))
P2

P3 <- ggplot(sensdataff,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=Periods)) + ylim(0, 30) +
      labs(y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'))
P3
Pf <- multiplot(P1,P2,P3,layout=matrix(c(1,2,3), byrow = TRUE),cols=2,fontsize = 24)
dev.off()

# Plot by season
sensdataff_DJF=sensdataff[which(sensdataff$Season=="Winter"),]
sensdataff_MAM=sensdataff[which(sensdataff$Season=="Spring"),]
sensdataff_JJA=sensdataff[which(sensdataff$Season=="Summer"),]
sensdataff_SON=sensdataff[which(sensdataff$Season=="Fall"),]

png(paste(outdir2,"03_",loc,"_sensors_boxplots_typo_season_day.png",sep=""),width=1200, height=1000)
P4 <- ggplot(sensdataff_DJF,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=DayType)) + ylim(0, 30) +
      labs(title="DJF",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P4

P5 <- ggplot(sensdataff_MAM,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=DayType)) + ylim(0, 30) +
      labs(title="MAM",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P5

P6 <- ggplot(sensdataff_JJA,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=DayType)) + ylim(0, 30) +
      labs(title="JJA",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      #scale_x_discrete(limits=c("IS","RB","T","UB"))+
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P6

P7 <- ggplot(sensdataff_SON,aes(x=Typology,y=PM2.5)) + geom_boxplot(aes(color=DayType)) + ylim(0, 30) +
      labs(title="SON",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P7
Pf <- multiplot(P4,P5,P6,P7,cols=2,fontsize = 24)
dev.off()

png(paste(outdir2,"03_",loc,"_sensors_boxplots_season_day_hours.png",sep=""),width=1200, height=1000)
P8 <- ggplot(sensdataff_DJF,aes(x=DayType,y=PM2.5)) + geom_boxplot(aes(color=Periods)) + ylim(0, 30) +
      labs(title="DJF",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P8

P9 <- ggplot(sensdataff_MAM,aes(x=DayType,y=PM2.5)) + geom_boxplot(aes(color=Periods)) + ylim(0, 30) +
      labs(title="MAM",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P9

P10 <- ggplot(sensdataff_JJA,aes(x=DayType,y=PM2.5)) + geom_boxplot(aes(color=Periods)) + ylim(0, 30) +
      labs(title="JJA",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P10

P11 <- ggplot(sensdataff_SON,aes(x=DayType,y=PM2.5)) + geom_boxplot(aes(color=Periods)) + ylim(0, 30) +
      labs(title="SON",y=bquote(PM2.5 ~ (mu*g/m^3))) +
      scale_color_manual(values=c("#0000CC", "#66CC00", "#FFCC33","#CC0000")) +
      theme(plot.title = element_text(size=18),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text = element_text(size =14),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position="top")
P11
Pf <- multiplot(P8,P9,P10,P11,cols=2,fontsize = 24)
dev.off()

######################################
#          CLASSIFICATION            #          
######################################
# Classification considering season only => 4*4 = 16 groups
Group <- rep("group",length(sensdataff[,1]))
sensdataff$Group=Group
sensdataff=subset(sensdataff,select=-c(DayType,Periods))

# Create groups
Groups <- crossing(var1 = sensdataff$Typology, var2 = sensdataff$Season, var3=sensdataff$Clust)#, var3 = sensdataff$DayType, var4 = sensdataff$Periods)

for (ngroup in 1:(dim(Groups)[1])){

    group_name=eval(paste0("G",ngroup))

    igroup = Groups[ngroup,]
    
    typo=igroup$var1
    season=igroup$var2
    #daytype=igroup$var3
    #periods=igroup$var4
    clust=igroup$var3

    idx=which(sensdataff$Typology==typo & sensdataff$Season==season & sensdataff$Clust==clust)#& sensdataff$DayTyp==daytype & sensdataff$Periods==periods)
    sensdataff$Group[idx]<-group_name

}

tmp=subset(sensdataff,select=c(Group))
tmp$Count=rep(1,length(tmp[,1]))
stat_group = aggregate(. ~ Group, tmp, function(x) sum(x, na.rm=TRUE), na.action = na.pass)

#####################################
#            SAVE DATA              #          
#####################################

save(sensdataff,file=paste0(outdir,"LCS_df_all_clean_groups.Rda"))

