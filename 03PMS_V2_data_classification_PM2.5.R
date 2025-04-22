rm(list = ls())

# Set directory
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1")


# Import libraries
library(dplyr)
library(data.table)
library(chron)
library(ggplot2)
library(stats)
library(RColorBrewer)
library(stringr)
library(sf)
library(raster)
library(maptools)
library(fields)
library(rgdal)
library(tidyr)
require(sp)
library(leaflet)
library(gridExtra)
library(mapview)

# Directory paths
indir   <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/"# path for input directory
outdir  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/"# path for output directory
outdir2  <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/"# path for output directory
outdir3 <-"C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/OUTPUTS1/figs1/outliers_detection TEST"

# Init variables
loc <-"Antwerp" # estimation location
pol <-"PM2.5" # pollutant



#####################################
#           LOAD SIG DATA           #          
#####################################

print("READ SIG DATA")

CRS_BELG <- CRS("+init=epsg:32631") 


# 
# # Définir le chemin d'accès au fichier shapefile
# shp_path <- "C:/Users/chaachay/Desktop/Stage_SensEURCity_final/INPUTS/SIG_data/boundaries/georef-belgium-province-millesime.shp"
# 
# # Lire les données du shapefile
# antw_bound <- st_read(shp_path)
# antw_bound <- st_transform(antw_bound,CRS_BELG)
# 
# 
# # Visualiser les limites géographiques avec ggplot
# ggplot(data = antw_bound) +
#   geom_sf() +
#   ggtitle("Limites géographiques de la province d'Anvers") +
#   theme_minimal()



#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(paste0(outdir,"LCS_PMS_df_all_clean.Rda"))
load("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/Stage_SensEURCity_final/INPUTS1/ref_df_all.Rda")
load(paste0(outdir,"PMS_V2_typo_CLC_BDD_comparison.Rda"))

ref_df_all <- ref_df_all %>% rename(Location.ID = ID)

### Reference data ###
ref_df_all=ref_df_all[ref_df_all$PM2.5 > 0,] # Only keep positive values
ID_ref=unique(ref_df_all$Location.ID)





#####################################
#         ASSIGN TYPOLOGY           #          
#####################################


print("START ASSIGN TYPOLOGY")

#ref
ref_df_all_wtypo <- left_join(ref_df_all, typo_comparison[, c("Location.ID", "Typology.sEURcity")], by = "Location.ID")
names(ref_df_all_wtypo)[names(ref_df_all_wtypo) == "Typology.sEURcity"] <- "Typology"
save(ref_df_all_wtypo,file=paste0(outdir,"PMS_ref_df_all_wtypo.Rda"))


#sens
dataout_wtypo <- left_join(dataout, typo_comparison[, c("Location.ID", "Typology.sEURcity")], by = "Location.ID")
names(dataout_wtypo)[names(dataout_wtypo) == "Typology.sEURcity"] <- "Typology"
save(dataout_wtypo,file=paste0(outdir,"LCS_PMS_df_all_clean_wtypo.Rda"))


stadata <- ref_df_all_wtypo

stadata2 = subset(stadata, select=-c(datetime))
stadata2$Location.ID=as.character(stadata2$Location.ID)
stadata2$Typology=as.character(stadata2$Typology)

stadata2 = aggregate(. ~ Location.ID+Typology, stadata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(stadata2) = stadata2$Location.ID
nbr_sta = nrow(stadata2)

### Sensor data ###
sensdata <- dataout_wtypo
sensdata <- as.data.frame(sensdata)
sensdata2 <- subset(sensdata, select = setdiff(names(sensdata), c("datetime", "Typology")))
sensdata2$ID=as.character(sensdata2$ID)
sensdata2$Location.ID=as.character(sensdata2$Location.ID)
sensdata2$PM2.5=as.numeric(sensdata2$PM2.5)

sensdata2 = aggregate(. ~ ID + Location.ID, sensdata2, function(x) mean(x, na.rm=TRUE), na.action = na.pass)
rownames(sensdata2) = sensdata2$ID
nbr_sensor = nrow(sensdata2)



typology_sens <- sensdata %>%
  dplyr::select(ID, Location.ID, Typology) %>%
  dplyr::distinct(ID, Location.ID, Typology)

typology_sens[which(typology_sens$Location.ID=="ANT_URB_KIPD"),3]<-"URB"

save(typology_sens,file=paste0(outdir,"PMS_typology_sens.Rda"))

sensdata2 <- left_join(sensdata2, typology_sens, by = c("ID", "Location.ID"))


# Define start and end date
start_date <- min(stadata$date) # starting date
end_date <-max(stadata$date) # ending date

sensdata3 <- sensdata2

# Création de la catégorie par défaut pour tous les capteurs
sensdata3$Category <- "dedicated"

# Capteurs "colocated"
colocated_ids <- paste0("Antwerp_", c("4065EA", "4067BD", "4067B3", "40642B", "4047D7", "4065DA"))
sensdata3$Category[sensdata3$ID %in% colocated_ids] <- "colocated"

# Capteurs "duplicated-colocated"
duplicated_colocated_ids <- paste0("Antwerp_", c("40499F", "4043AE", "4049A6", "4043A7", "40499C", "4043B1"))
sensdata3$Category[sensdata3$ID %in% duplicated_colocated_ids] <- "duplicated-colocated"


# sensdata3 en un sf object
spsensdata3_sf <- st_as_sf(sensdata3, coords = c("X", "Y"), crs = CRS_BELG)

# coordonnées en WGS84
spsensdata3_sf <- st_transform(spsensdata3_sf, crs = 4326)

# coordonnées pour leaflet
coords <- st_coordinates(spsensdata3_sf)
spsensdata3_sf$lon <- coords[, 1]
spsensdata3_sf$lat <- coords[, 2]


# palette de couleurs
colors <- colorFactor(palette = c("darkblue", "darkred", "darkgreen"), domain = spsensdata3_sf$Typology)


# carte interactive
m_typo <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = spsensdata3_sf, ~lon, ~lat, popup = ~as.character(Typology),
                   color = ~colors(Typology), fill = TRUE, radius = 5) %>%
  addControl(html = "
    <div style='background: white; padding: 10px;'>
      <h4>Typology</h4>
      <div style='display: flex; align-items: center;'>
        <div style='width: 15px; height: 15px; background: darkblue; margin-right: 5px;'></div>
        URB
      </div>
      <div style='display: flex; align-items: center;'>
        <div style='width: 15px; height: 15px; background: darkred; margin-right: 5px;'></div>
        TRA
      </div>
      <div style='display: flex; align-items: center;'>
        <div style='width: 15px; height: 15px; background: darkgreen; margin-right: 5px;'></div>
        INDUS
      </div>
    </div>",
             position = "bottomright"
  )

m_typo
mapshot(m_typo, file = paste0(outdir2, "/03PMS_Map_Antwerp_Sensors_v2.png"))


pal <- colorFactor(palette = c("blue", "red", "darkgreen"), 
                   domain = c("dedicated", "colocated", "duplicated-colocated"))

# carte
m_category <- leaflet(spsensdata3_sf) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, color = ~pal(Category), 
                   popup = ~paste("ID:", ID, "<br>Category:", Category)) %>%
  addLegend("bottomright", pal = pal, values = ~Category, title = "Sensor Category")

m_category
mapshot(m_category, file = paste0(outdir2, "/03PMS_Map_Antwerp_Sensors_Category.png"))






# 
# # Check typology assignment / reference station
# 
# #Calculate distance-matrix between reference stations and sensors
# distancesSensorReference = data.frame()
# 
# for(referenceInt in 1:nrow(stadata2)){
#   referenceRow = stadata2[referenceInt,]
#   currentRow = t(data.frame(
#     sqrt((sensdata2$X - referenceRow$Ref.Long)^2 +
#            (sensdata2$Y - referenceRow$Ref.Lat)^2)
#   ))
#   colnames(currentRow) = sensdata2$ID
#   rownames(currentRow) = referenceRow$Location.ID
#   distancesSensorReference = rbind(distancesSensorReference,currentRow)
# }
# 
# 
# 
# stadata2$Representativity_max <- 50
# 
# 
# 
# 
# # # Select groups of sensors around reference stations by providing:
# # # 1) minCountSensor: a minimum number of sensors
# # # 2) maxCountSensor: providing a maximum number of sensors
# # # 3) maxTolDistSensor: providing a maximum tolerable distance between sensor and reference station that depends on the station typology = representativeness
# minCountSensor = 1
# maxCountSensor = 1000
# 
# listSensorCalibrationGroups = list()
# vectorGroupIDs = c()
# allCloseSensors = c()
# iList = 1
# 
# NotSelectedStadata<-c()
# 
# 
# 
# for (iRef in 1:nbr_sta){
#   #select all sensors within maxTolDistSensor from reference station. And sort by distance, starting close
#   maxTolDistSensor = stadata2$Representativity_max[iRef]
#   print('je suis la ')
#   
#   nrvalid=length(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= maxTolDistSensor])
#   if(nrvalid> 0){
#     
#     row <- sapply(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= maxTolDistSensor], as.numeric)
#     closeSensors =  sort(row)
#     closeSensors
#     if(nrvalid==1){
#       row2 <- sapply(distancesSensorReference[iRef,][,distancesSensorReference[iRef,]<= 5000], as.numeric)
#       closeSensors =  sort(row2)
#       closeSensors = closeSensors[1]
#     }
#     
#     referenceID = as.character(stadata2[iRef,]$Location.ID)
#     #select a maximum of maxCountSensor sensors
#     
#     closeSensors = closeSensors[1:min(length(closeSensors),maxCountSensor)]
#     
#     #only create a group of sensors if more than minCountSensor sensors are available
#     if (nrvalid >=  minCountSensor) {
#       vectorGroupIDs[iList] = referenceID
#       listSensorCalibrationGroups[[iList]] = list(referenceID,
#                                                   closeSensors)
#       allCloseSensors = c(allCloseSensors,
#                           names(closeSensors)
#       )
#       iList = iList + 1
#     }else {
#       
#       NotSelectedStadata_tmp <- stadata2[which(stadata2$Location.ID==referenceID),]
#       NotSelectedStadata <- rbind(NotSelectedStadata,NotSelectedStadata_tmp)
#       
#       
#     }
#     
#   }
# }
# 
# 
# #Sensors can be colocated with multiple reference locations. 
# allCloseSensors = unique(allCloseSensors)
# print(vectorGroupIDs)
# print(allCloseSensors)
# print(listSensorCalibrationGroups)



######################################
#            CLUSTER                 #          
######################################


spsensdata2clust <- sensdata2
coordinates(spsensdata2clust)=~X+Y
proj4string(spsensdata2clust)=CRS_BELG
class(spsensdata2clust); summary(spsensdata2clust)


# Convertir votre data frame sensdata2 en un sf 
spsensdata2clust_sf <- st_as_sf(spsensdata2clust, coords = c("X", "Y"), crs = CRS_BELG)

# Transformer les coordonnées en WGS84 (utilisé par Leaflet)
spsensdata2clust_sf <- st_transform(spsensdata2clust_sf, crs = 4326)

coords <- st_coordinates(spsensdata2clust_sf)
spsensdata2clust_sf$lon <- coords[, 1]
spsensdata2clust_sf$lat <- coords[, 2]


# Look for clusters
library(fastcluster)
chc <- hclust(dist(data.frame(rownames=rownames(spsensdata2clust@data), x=coordinates(spsensdata2clust)[,1],
                              y=coordinates(spsensdata2clust)[,2])), method="complete")

chc.d50 <- cutree(chc, h=50) 
spsensdata2clust@data <- data.frame(spsensdata2clust@data, Clust=chc.d50)
spsensdata2clust_sf$Clust <- as.factor(chc.d50)  # Convert clusters to factor for coloring

nb.cols <- max(spsensdata2clust@data$Clust)
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
pal_cluster <- colorFactor(palette = mycolors, domain = spsensdata2clust_sf$Clust)

m_cluster <- leaflet(spsensdata2clust_sf) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, color = ~pal_cluster(Clust), radius = 5, fillOpacity = 0.7) %>%
  addLegend("bottomright", pal = pal_cluster, values = ~Clust, title = "SensorClusters")

m_cluster
mapshot(m_cluster, file = paste0(outdir2, "03PMS_Map_Antwerp_Sensor_Clusters.png"))


###############

df_clusters=as.data.frame(spsensdata2clust)

sensdata_wtypo_corr_cluster = as.data.frame(spsensdata2clust)
sensdata_wtypo_corr_cluster = subset(sensdata_wtypo_corr_cluster,select=-c(X,Y,PM2.5,Typology,Location.ID))

sensdata_wtypo_corr_cluster2=sensdata_wtypo_corr_cluster
sensdata_wtypo_corr_cluster2 = subset(sensdata_wtypo_corr_cluster2,select=-c(ID))
sensdata_wtypo_corr_cluster2$Count=rep(1,length(sensdata_wtypo_corr_cluster2[,1]))
stat_clust = aggregate(. ~ Clust, sensdata_wtypo_corr_cluster2, function(x) sum(x, na.rm=TRUE), na.action = na.pass)
stat_clust = subset(stat_clust,select=c(Clust,Count))
clust2out = which(stat_clust$Count > 10)

sensdataf2=merge(sensdata,sensdata_wtypo_corr_cluster,by=c("ID"))


######################################
#          ASSIGN SEASON             #          
######################################

winter_start = as.POSIXct("2020-12-01 00:00:00", tz="UTC")
winter_end = as.POSIXct("2021-02-28 23:59:59", tz="UTC")

# Spring (MAM)
spring_start = as.POSIXct("2021-03-01 00:00:00", tz="UTC")
spring_end = as.POSIXct("2021-05-31 23:59:59", tz="UTC")

# Summer (JJA)
summer_start = as.POSIXct("2020-06-01 00:00:00", tz="UTC")
summer_end = as.POSIXct("2020-08-31 23:59:59", tz="UTC")

# Fall (SON)
fall_start = as.POSIXct("2020-09-01 00:00:00", tz="UTC")
fall_end = as.POSIXct("2020-11-30 23:59:59", tz="UTC")

# Créer une nouvelle colonne 'Season' avec une valeur par défaut
sensdataff<-sensdataf2
sensdataff$Season <- "no season"

# Assigner les saisons
sensdataff$Season[which(sensdataff$datetime >= winter_start & sensdataff$datetime <= winter_end)] <- "Winter"
sensdataff$Season[which(sensdataff$datetime >= spring_start & sensdataff$datetime <= spring_end)] <- "Spring"
sensdataff$Season[which(sensdataff$datetime >= summer_start & sensdataff$datetime <= summer_end)] <- "Summer"
sensdataff$Season[which(sensdataff$datetime >= fall_start & sensdataff$datetime <= fall_end)] <- "Fall"



######################################
#     ASSIGN WEEK DAYS/ WEEKENDS     #          
######################################

Weekdays <- rep("weekdays",length(sensdataff[,1]))
sensdataff$Weekdays=Weekdays
sensdataff$Weekdays=weekdays(as.POSIXct(sensdataff$datetime), abbreviate = F)

DayType <- rep("Weekday",length(sensdataff[,1]))
sensdataff$DayType=DayType

sensdataff[which(sensdataff$Weekdays == "samedi" | sensdataff$Weekdays == "dimanche"),11]<-"Weekend"


######################################
#           ASSIGN PERIODS           #          
######################################

sensdataff$Hour=as.numeric(format(sensdataff$datetime, format = "%H"))
Periods <- rep("periods",length(sensdataff[,1]))
sensdataff$Periods=Periods

sensdataff[which(sensdataff$Hour > 6 & sensdataff$Hour <= 9 | sensdataff$Hour > 16 & sensdataff$Hour <= 20),13]<-"Traffic hours"
sensdataff[which(sensdataff$Hour > 9 & sensdataff$Hour <= 16 | sensdataff$Hour > 20 & sensdataff$Hour <= 22),13]<-"Off-peak hours"
sensdataff[which(sensdataff$Hour > 22 | sensdataff$Hour == 0 | sensdataff$Hour > 5 & sensdataff$Hour <= 6),13]<-"Transition periods"
sensdataff[which(sensdataff$Hour >= 1 & sensdataff$Hour <= 5),13]<-"Night hours"

sensdataff=subset(sensdataff,select=-c(Hour,Weekdays))
sensdataff <- sensdataff[!is.na(sensdataff$Typology), ]
unique(sensdataff$Typology)
# Plot
sensdataff$PM2.5 <- as.numeric(sensdataff$PM2.5)
sensdataff <- sensdataff[!is.infinite(sensdataff$PM2.5) & !is.na(sensdataff$PM2.5), ]


#boxplot limité aux valeurs inférieur à 30 

# # Boxplot pour 'Season'
# P1 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Season)) +
#   geom_boxplot() +
#   ylim(0, 30) +
#   labs(y = "PM2.5 (µg/m³)") +
#   scale_color_manual(values = c("Winter" = "#0000CC", "Spring" = "#66CC00", "Summer" = "#FFCC33", "Fall" = "#CC0000")) +
#   theme_minimal()
# 
# # Boxplot pour 'DayType'
# P2 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = DayType)) +
#   geom_boxplot() +
#   ylim(0, 30) +
#   labs(y = "PM2.5 (µg/m³)") +
#   scale_color_manual(values = c("Weekday" = "#0000CC", "Weekend" = "#66CC00")) +
#   theme_minimal()
# 
# # Boxplot pour 'Periods'
# P3 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Periods)) +
#   geom_boxplot() +
#   ylim(0, 30) +
#   labs(y = "PM2.5 (µg/m³)") +
#   scale_color_manual(values = c("Night hours" = "#0000CC", "Off-peak hours" = "#66CC00", "Traffic hours" = "#FFCC33", "Transition periods" = "#CC0000")) +
#   theme_minimal()
# 
# # Combine les graphiques en un seul output
# png(filename = paste0(outdir2, "03_", loc, "_sensors_boxplots_typo_season_day_hour_final_v2.png"), width = 800, height = 700)
# grid.arrange(P1, P2, P3, ncol = 1)
# dev.off()


summary(sensdataff)

# Boxplot pour 'Season'
P1 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Season)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)") +
  scale_color_manual(values = c("Winter" = "#0000CC", "Spring" = "#66CC00", "Summer" = "#FFCC33", "Fall" = "#CC0000")) +
  theme_minimal()

# Boxplot pour 'DayType'
P2 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = DayType)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)") +
  scale_color_manual(values = c("Weekday" = "#0000CC", "Weekend" = "#66CC00")) +
  theme_minimal()

# Boxplot pour 'Periods'
P3 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Periods)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)") +
  scale_color_manual(values = c("Night hours" = "#0000CC", "Off-peak hours" = "#66CC00", "Traffic hours" = "#FFCC33", "Transition periods" = "#CC0000")) +
  theme_minimal()

png(filename = paste0(outdir2, "03PMS_", loc, "_sensors_boxplots_typo_season_day_hour_final_v3.png"), width = 800, height = 700)
grid.arrange(P1, P2, P3, ncol = 1)
dev.off()



# Log des boxplots
sensdataff_clean <- sensdataff %>%
  filter(PM2.5 > 0)

Log_sensdataff <- sensdataff %>%
  mutate(logPM25 = log(PM2.5 + 1))

# === Boxplot logPM2.5 par Typology et Season ===
P4 <- ggplot(Log_sensdataff, aes(x = Typology, y = logPM25, color = Season)) +
  geom_boxplot() +
  labs(y = "log(PM2.5 + 1)") +
  scale_color_manual(values = c("Winter" = "#0000CC", "Spring" = "#66CC00", "Summer" = "#FFCC33", "Fall" = "#CC0000")) +
  theme_minimal()

# === Boxplot logPM2.5 par Typology et DayType ===
P5 <- ggplot(Log_sensdataff, aes(x = Typology, y = logPM25, color = DayType)) +
  geom_boxplot() +
  labs(y = "log(PM2.5 + 1)") +
  scale_color_manual(values = c("Weekday" = "#0000CC", "Weekend" = "#66CC00")) +
  theme_minimal()

# === Boxplot logPM2.5 par Typology et Periods ===
P6 <- ggplot(Log_sensdataff, aes(x = Typology, y = logPM25, color = Periods)) +
  geom_boxplot() +
  labs(y = "log(PM2.5 + 1)") +
  scale_color_manual(values = c("Night hours" = "#0000CC", "Off-peak hours" = "#66CC00", 
                                "Traffic hours" = "#FFCC33", "Transition periods" = "#CC0000")) +
  theme_minimal()


png(filename = paste0(outdir2, "03PMS_", loc, "_logPM25_boxplots_typo_season_day_hour.png"), width = 800, height = 700)
gridExtra::grid.arrange(P4, P5, P6, ncol = 1)
dev.off()


# Echelle Log

# "Season"
P7 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Season)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)", title = "PM2.5 selon la saison") +
  scale_y_log10() +
  scale_color_manual(values = c("Winter" = "#0000CC", "Spring" = "#66CC00", 
                                "Summer" = "#FFCC33", "Fall" = "#CC0000")) +
  theme_minimal()

# "DayType"
P8 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = DayType)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)", title = "PM2.5 selon le type de jour") +
  scale_y_log10() +
  scale_color_manual(values = c("Weekday" = "#0000CC", "Weekend" = "#66CC00")) +
  theme_minimal()

# "Periods"
P9 <- ggplot(sensdataff, aes(x = Typology, y = PM2.5, color = Periods)) +
  geom_boxplot() +
  labs(y = "PM2.5 (µg/m³)", title = "PM2.5 selon les périodes horaires") +
  scale_y_log10() +
  scale_color_manual(values = c("Night hours" = "#0000CC", "Off-peak hours" = "#66CC00", 
                                "Traffic hours" = "#FFCC33", "Transition periods" = "#CC0000")) +
  theme_minimal()


png(filename = paste0(outdir2, "03PMS_", loc, "_boxplots_logScale_typo_season_day_hour.png"), 
    width = 800, height = 700)
gridExtra::grid.arrange(P7, P8, P9, ncol = 1)
dev.off()



######################################
#          CLASSIFICATION            #          
######################################
# Classification considering season only => 4*4 = 16 groups
Group <- rep("group",length(sensdataff[,1]))
sensdataff$Group=Group
# sensdataff=subset(sensdataff,select=-c(DayType,Periods))


# Create groups
Groups <- crossing(var1 = sensdataff$Typology, var2 = sensdataff$Season, var3=sensdataff$Clust ,var4 = sensdataff$DayType, var5 = sensdataff$Periods)

for (ngroup in 1:(dim(Groups)[1])){
  
  group_name=eval(paste0("G",ngroup))
  
  igroup = Groups[ngroup,]
  
  typo=igroup$var1
  season=igroup$var2
  clust=igroup$var3
  daytype=igroup$var4
  periods=igroup$var5
  
  idx=which(sensdataff$Typology==typo & sensdataff$Season==season 
            & sensdataff$Clust==clust & sensdataff$DayType==daytype & sensdataff$Periods==periods)
  sensdataff$Group[idx]<-group_name
  
}

tmp=subset(sensdataff,select=c(Group))
tmp$Count=rep(1,length(tmp[,1]))
stat_group = aggregate(. ~ Group, tmp, function(x) sum(x, na.rm=TRUE), na.action = na.pass)

#####################################
#            SAVE DATA              #          
#####################################
LCS_df_all_clean_groups <- sensdataff
save(LCS_df_all_clean_groups,file=paste0(outdir,"LCS_df_all_clean_groups.Rda"))
