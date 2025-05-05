rm(list = ls())

# Charger le fichier de configuration global
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

# Import libraries
library(dplyr)
library(data.table)
library(chron)
library(ggplot2)
library(stats)
library(RColorBrewer)
library(sf)


#################################################################################
#                            SENSOR DATA CLEANING                               #
#                  Created 22/06/2021 updated on 25/08/2021                     #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################

# Chargement des données
ref_df_all <- fread(file_ref_df_all_csv)
save(ref_df_all, file = file_ref_df_all_rda)

LCS_df_all1 <- fread(file_lcs_df_all_csv)
save(LCS_df_all1, file = file_lcs_df_all_rda)
LCS_df_all <- LCS_df_all1

#renommage de colonne pour etre adapté au code d'Alicia 
ref_df_all <- as.data.frame(ref_df_all)
ref_df_all <- ref_df_all %>%
  rename(
    X = Ref.Long,
    Y = Ref.Lat,
    datetime = date,
    ID = Location.ID,
    PM2.5 = Ref.PM2.5,
    PM10 = Ref.PM10
  )


LCS_df_all <- as.data.frame(LCS_df_all)
LCS_df_all <- LCS_df_all %>%
  rename(
    X = longitude,
    Y = latitude,
    datetime = date,
    PM2.5 = PMS_PM25,
    PM10 = PMS_PM10
  )

LCS_df_all <- as.data.frame(LCS_df_all)

# Fonction conversion coordonnées
convert_coords <- function(df, lon, lat) {
  df <- as.data.frame(df)
  df_sf <- st_as_sf(df, coords = c(lon, lat), crs = 4326)
  df_sf_meters <- st_transform(df_sf, crs = 32631)
  df_transformed <- cbind(df, st_coordinates(df_sf_meters))
  return(df_transformed)
}

ref_df_all <- ref_df_all[!is.na(ref_df_all$X) & !is.na(ref_df_all$Y), ]

# Application de la fonction de conversion sur ref_df_all et LCS_df_all
ref_df_all <- convert_coords(ref_df_all, "X", "Y")
ref_df_all$X <- ref_df_all$X.1
ref_df_all$Y <- ref_df_all$Y.1

# # Supprimer les lignes avec des valeurs manquantes dans les colonnes spécifiées
LCS_df_all <- LCS_df_all[!is.na(LCS_df_all$X) & !is.na(LCS_df_all$Y), ]

LCS_df_all <- convert_coords(LCS_df_all, "X", "Y")
LCS_df_all$X <- LCS_df_all$X.1
LCS_df_all$Y <- LCS_df_all$Y.1

### Reference data ###
ref_df_all=ref_df_all[ref_df_all$PM2.5 > 0,] # Only keep positive values
ID_ref=unique(ref_df_all$ID)
ref_df_all<- na.omit(ref_df_all)

# Typology
station_typo <- read.csv(file=file_station_typology, header=TRUE, sep=";",skip=0)
colnames(station_typo)[1] <- "ID"
names(station_typo)[names(station_typo) == "PM2.5"] <- "PM2.5_typo"
names(station_typo)[names(station_typo) == "PM10"] <- "PM10_typo"
station_typo <- subset(station_typo, select=-c(Organisation,PM10_typo))
station_typo$PM2.5_typo[station_typo$PM2.5_typo==-999]<-NA
stadata <- merge(ref_df_all,station_typo,by="ID") # Merde ref data and typo
stadata=subset(stadata,select=-c(Representativity_max,PM2.5_typo))
stadata <- subset(ref_df_all, select=-c(PM10))

ref_df_all <- ref_df_all %>%
  mutate(Representativity_min = 100)

### Sensor data ###
sensdata <- LCS_df_all
# sensdata = subset(sensdata,select=-c(PM2.5_real,PM2.5_calib,USE,TSU))
sensdata = subset(sensdata,select=-c(PM10))
sensdata$PM2.5=as.numeric(sensdata$PM2.5)
sensdata$X=as.numeric(sensdata$X)
sensdata$Y=as.numeric(sensdata$Y)
sensdata=sensdata[which(!(is.na(sensdata$Y))),]

# Define start and end date
start_date <- min(stadata$datetime) # starting date
end_date <-max(stadata$datetime) # ending date

#####################################
#       DISTANCE REF/SENSORS        #          
#####################################

#Calculate distance-matrix between reference stations and sensors
# stadata2 = subset(stadata, select=-c(Typology,datetime))
stadata2 = subset(stadata, select=-c(datetime))
stadata2$ID=as.character(stadata2$ID)
stadata2 = aggregate(. ~ ID, stadata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(stadata2) = stadata2$ID
stadata2 <- stadata2[-1, ]
nbr_sta = nrow(stadata2)



sensdata2 = subset(sensdata, select=-c(datetime))
sensdata2$ID=as.character(sensdata2$ID)
sensdata2$Location.ID=as.character(sensdata2$Location.ID)
# à revoir car X et Y constantes alors qu'ils ne sont pas censés l'être 
sensdata2$PM2.5=as.numeric(sensdata2$PM2.5)
sensdata2$X=as.numeric(sensdata2$X)
sensdata2$Y=as.numeric(sensdata2$Y)

sensdata2 = aggregate(. ~ ID + Location.ID, sensdata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(sensdata2) = sensdata2$ID
nbr_sensor = nrow(sensdata2)

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

#####################################
#           SENSOR GROUPS           #          
#####################################

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

for (iRef in 1:9){
  #select all sensors within maxTolDistSensor from reference station. And sort by distance, starting close
  maxTolDistSensor = stadata2$Representativity_min[iRef]
  nrvalid=length(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= maxTolDistSensor])
  if(nrvalid> 0){
    
    row <- sapply(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= maxTolDistSensor], as.numeric)
    closeSensors =  sort(row)
    closeSensors
    if(nrvalid==1){
      row2 <- sapply(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= 5000], as.numeric)
      closeSensors =  sort(row2)
      closeSensors = closeSensors[1]
    }
    
    referenceID = as.character(stadata2[iRef,]$ID)
    #select a maximum of maxCountSensor sensors
    
    closeSensors = closeSensors[1:min(length(closeSensors),maxCountSensor)]
    
    #only create a group of sensors if more than minCountSensor sensors are available
    if (nrvalid >=  minCountSensor) {
      vectorGroupIDs[iList] = referenceID
      listSensorCalibrationGroups[[iList]] = list(referenceID,
                                                  closeSensors)
      allCloseSensors = c(allCloseSensors,
                          names(closeSensors)
      )
      iList = iList + 1
    }
  }
}


#Sensors can be colocated with multiple reference locations. 
allCloseSensors = unique(allCloseSensors)
print(vectorGroupIDs)
print(allCloseSensors)
print(listSensorCalibrationGroups)
#####################################
#         CLEANING DATA             #          
#####################################

#1) Eliminate negative values
print("#1) Eliminate negative values")
sensdata=sensdata[which(sensdata$PM2.5>=0),]

#2) Eliminate values > threshold value based on max reference station value
print("#2) Eliminate values > threshold value based on max reference station value")
MaxValue=max(ref_df_all$PM2.5)
sensdata=sensdata[which(sensdata$PM2.5<=2*MaxValue),]

#3) Identify frozen values (several hours / days)
print("#3) Identify frozen values (several hours / days)")
sensdata$ID=as.character(sensdata$ID)

# Loop over sensors
allsensdata <- c()
data_clean=0

for (isens in 1:nbr_sensor){
  #print(paste0("Sensor N°",isens))
  
  # Load the ith sensor data
  isensID=sensdata2$ID[isens]
  isensdata=sensdata[which(sensdata$ID==isensID),]
  isensdata2=isensdata
  
  if (length(isensdata[,1])>1){
    # Init count of frozen values
    count=0
    
    # Loop over observations
    for (i in 2:length(isensdata[,1])){ 
      idata=isensdata$PM2.5[i]
      jdata=isensdata$PM2.5[i-1]
      
      if (idata==jdata){
        count=count+1
      }else{
        if (count>=3){
          #print("cleaning data")
          data_clean=data_clean+count
          l1=i-count-1
          l2=l1+count
          isensdata2[l1:l2,4]<-NA
        }  
        count=0
      }
    } # end loop over observations
    
    # Storage
    if (isens==1){
      allsensdata <- isensdata2
    }else{ 
      allsensdata <- rbind(allsensdata,isensdata2)
    }
    
  }else{
    
    allsensdata <- rbind(allsensdata,isensdata2)
  } # if isensdata > 1
} # end loop over sensors

#allsensdata = na.omit(allsensdata)
allsensdata2=allsensdata
allsensdata2=allsensdata2[complete.cases(allsensdata2$PM2.5),]

#4) Eliminate sensor with positive constant bias
print("#4) Eliminate sensor with positive constant bias")

count=0
ID_2_REMOVE_all<-c()

#Loop on Ref stations with sensor group
for(listItem in listSensorCalibrationGroups){
  
  count=count+1
  
  print(count)
  
  referenceID = listItem[[1]][1]
  referenceSet = stadata
  referenceSubSet = referenceSet[which(referenceSet$ID==referenceID),]
  
  # Select sensor ID's in Group
  df_sensors = as.data.frame(listItem[[2]])
  sensorIDs = rownames(df_sensors)
  sensdataSubSet<-c()
  
  # Initialisez une liste pour collecter les data frames temporaires
  sensdataSubSetList <- list()
  
  for (nsensor in 1:length(sensorIDs)) {
    sensdataSubSetTmp = allsensdata2[which(allsensdata2$ID == sensorIDs[nsensor]), ]
    sensdataSubSetTmp$Dist = rep(df_sensors[nsensor,], length(sensdataSubSetTmp[, 1]))
    # Ajoutez le data frame temporaire à la liste
    sensdataSubSetList[[length(sensdataSubSetList) + 1]] <- sensdataSubSetTmp
  }
  
  # Après la boucle, combinez tous les data frames de la liste en un seul
  sensdataSubSet <- rbindlist(sensdataSubSetList, fill = TRUE)
  
  
  nbr_sensors = length(unique(sensdataSubSet$ID))
  
  if (length(sensdataSubSet[,1]) > 0){
    
    referenceSubSet=subset(referenceSubSet, select=-c(Representativity_min))
    referenceSubSet$ID=rep("REF",length(referenceSubSet[,1]))
    referenceSubSet$Dist=rep(0,length(referenceSubSet[,1]))
    referenceSubSet$Location.ID=rep(listItem[[1]][1],length(referenceSubSet[,1]))
    
    # data=rbind(referenceSubSet,sensdataSubSet) #create only one df with ref and sensor data
    data <- bind_rows(referenceSubSet, sensdataSubSet)
    
    # Correct sensor data with offset => calculate offset from ref
    med_REF <- median(referenceSubSet$PM2.5)
    min_REF <- min(referenceSubSet$PM2.5)
    med_sens_all <- c(); name_sens_all <- c(); med_diff_all <- c(); min_sens_all <- c(); min_diff_all <- c()
    
    for (isens in 1:nbr_sensors){
      name_isens <- sensorIDs[isens]
      isensdata <- data[which(data$ID==name_isens),]
      med_isens <- median(isensdata$PM2.5, na.rm=TRUE)
      min_isens <- min(isensdata$PM2.5, na.rm=TRUE)
      min_diff <- min_isens-min_REF
      med_diff <- med_isens-med_REF
      #concatenate over sensors
      med_sens_all <- rbind(med_sens_all,med_isens)
      min_sens_all <- rbind(min_sens_all,min_isens)
      name_sens_all <- rbind(name_sens_all,name_isens)
      med_diff_all <- rbind(med_diff_all,med_diff)
      min_diff_all <- rbind(min_diff_all,min_diff)
    }
    
    rownames(med_sens_all)=name_sens_all
    med_sens_allf <- as.data.frame(cbind(med_sens_all,med_diff_all,min_sens_all,min_diff_all))
    colnames(med_sens_allf)[1]<- "MEDIAN"; colnames(med_sens_allf)[2]<- "OFFSET"
    colnames(med_sens_allf)[3]<- "MIN"; colnames(med_sens_allf)[4]<- "MIN_DIFF"
    idx_med=which(med_sens_allf$OFFSET>=(3*med_REF))
    ID_2_REMOVE=rownames(med_sens_allf[idx_med,])
    
    if (length(ID_2_REMOVE>1)){    
      ID_2_REMOVE_all=rbind(ID_2_REMOVE_all,ID_2_REMOVE)
      
      #Plot
      print("plot start")
      nbr_colors = nbr_sensors
      group.colors <- colorRampPalette(brewer.pal(12, "Paired"))(nbr_colors+1)
      group.colors[2]="#000000"
      
      # png(paste0(outdir2,count,"_REF_sensor_trace.png"),width=1200, height=400)
      # print(P <- ggplot(data,aes(x=datetime,y=PM2.5,fill=ID)) + geom_line(aes(color=ID),size=0.7) + #ylim(0, 20) +
      #         scale_color_manual(values=group.colors) +
      #         labs(title="",y=bquote(PM2.5 ~ (mu*g/m^3)),x="") +
      #         theme_bw()+
      #         theme_minimal()+
      #         theme(plot.title = element_text(size=18),
      #             axis.text=element_text(size=18),
      #             axis.title=element_text(size=18),
      #             legend.text = element_text(size =18),
      #             legend.title = element_blank(),
      #             legend.spacing.x = unit(0.3, 'cm'),
      #             legend.position= "top")
      # )
      # P
      # dev.off()
      
    }
    
  } # end if sens data exist
} # end loop over groups

rownames(ID_2_REMOVE_all) <- NULL
dataout <- allsensdata2[!(allsensdata2$ID %in% ID_2_REMOVE_all),]

#####################################
#            SAVE DATA              #          
#####################################
write.csv(ref_df_all, file_ref_df_all_csv, row.names = FALSE)
save(ref_df_all, file = file_ref_df_all_rda)
save(dataout, file = file_LCS_df_all_clean_Rda)

#####################################
#           DATA STATS              #          
#####################################

df<-dataout
png(filename=file.path(path_figures_general, "02_Distribution_PM25_conc_clean.png"), width=600, height=600, type="cairo",bg = "white")
p1 <- ggplot(df, aes(x=PM2.5)) + geom_histogram(color="#333333",fill="#333333",alpha=0.5) +
  labs(title="",x=bquote(.(pollutant_name) ~ (mu*g/m^3)), y = "Frequency")+
  theme_bw()+
  theme_minimal()+
  theme(plot.title = element_text(size=24),
        axis.text=element_text(size=24),
        axis.title=element_text(size=24),
        legend.text = element_text(size =24),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.position= "top")
p1
dev.off()

