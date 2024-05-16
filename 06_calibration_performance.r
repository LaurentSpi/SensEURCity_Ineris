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
#                  OUTLIERS DETECTION / CALIBRATION PERFORMANCE                 #
#                               Created 25/03/2022                              #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################

# Directory paths
indir   <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/INPUTS/"# path for input directory
outdir  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/"# path for output directory
outdir2  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/figs/"# path for output directory

# Init variables 
loc <-"Netherlands" # estimation location
pol <-"PM25" # pollutant

#####################################
#            READ DATA              #          
#####################################

# Load raw data
load(paste0(indir,"LCS_df_all.Rda"))
idx=which(LCS_df_all$PM2.5_calib <= -900)
LCS_df_all$PM2.5_calib[idx]<-NA

load(paste0(indir,"ref_df_all.Rda"))

# Load calibrated data
calibratedSensors <- read.csv(file = paste0(outdir,"outliers_v2_calibratedSensors_nBootStrap_100_alltime_nmax1000_distmaxRepmax_outliers.csv"))
names(calibratedSensors)[names(calibratedSensors) == "CalibratedPM2.5"] <- "PM2.5_calib2"
calibratedSensors$datetime=as.POSIXct(calibratedSensors$datetime, origin="1970-01-01",tz="CET")

select_time_start=min(calibratedSensors$datetime)
select_time_end=max(calibratedSensors$datetime)

LCS_df_all <- LCS_df_all[which(LCS_df_all$datetime>=select_time_start & LCS_df_all$datetime<=select_time_end),]

# Merge => df
df <- merge(calibratedSensors,LCS_df_all,by=c("ID","X","Y","datetime","PM2.5"),all=TRUE)
df=na.omit(df)

# Time 
start_date=as.character(min(df$datetime))
end_date=as.character(max(df$datetime))
hours<-unique(df$datetime)
nbr_hours <- length(unique(df$datetime))

#####################################
#           PERFORMANCE             #          
#####################################

# Init
metrics_final <- c()

# Parameters
beta=2 # As defined in the delta tool
alpha=0.30
RV=25
U95r=0.36

for (i in 1:nbr_hours){

	Time=hours[i]
	Time_chr=as.character(Time)

	PM25_real=df[which(df$datetime==Time),9]
	PM25_calibRIVM=df[which(df$datetime==Time),10]
	PM25_calib=df[which(df$datetime==Time),6]
        PM25_raw=df[which(df$datetime==Time),5]

        U95=U95r*sqrt((1-alpha**2)*PM25_real + (alpha**2*RV**2))
        RMSU=sqrt(sum(U95**2)/length(PM25_real))

	# Raw sensor data
 	R2 = round(cor(PM25_real,PM25_raw),2)
	RMSE <- sqrt(sum((PM25_raw-PM25_real)**2)/length(PM25_real))
	MBE <- (sum(PM25_raw-PM25_real))/length(PM25_real)
	MAE <- (sum(abs(PM25_raw-PM25_real)))/length(PM25_real)
	#MQI=abs(df$PM2.5_real-df$PM2.5_calib)/(beta*df$U95)
	MQI = RMSE/(beta*RMSU)
	CASE="RAW"
        metrics_tmp1 <- as.data.frame(cbind(Time_chr,R2,MBE,MAE,RMSE,MQI,CASE))

	# Calibrated sensor data
        R2 = round(cor(PM25_real,PM25_calib),2)
        RMSE <- sqrt(sum((PM25_calib-PM25_real)**2)/length(PM25_real))
        MBE <- (sum(PM25_calib-PM25_real))/length(PM25_real)
        MAE <- (sum(abs(PM25_calib-PM25_real)))/length(PM25_real)
        MQI = RMSE/(beta*RMSU)
        CASE="CALIB"
        metrics_tmp2 <- as.data.frame(cbind(Time_chr,R2,MBE,MAE,RMSE,MQI,CASE))

        # RIVM Calibrated sensor data
        R2 = round(cor(PM25_real,PM25_calibRIVM),2)
        RMSE <- sqrt(sum((PM25_calibRIVM-PM25_real)**2)/length(PM25_real))
        MBE <- (sum(PM25_calibRIVM-PM25_real))/length(PM25_real)
        MAE <- (sum(abs(PM25_calibRIVM-PM25_real)))/length(PM25_real)
        MQI = RMSE/(beta*RMSU)
        CASE="CALIB_RIVM"
        metrics_tmp3 <- as.data.frame(cbind(Time_chr,R2,MBE,MAE,RMSE,MQI,CASE))

	metrics <- rbind(metrics_tmp1,metrics_tmp2,metrics_tmp3)
	metrics_final <- rbind(metrics_final,metrics)

}

# Do correlation plots
correlation_raw = round(cor(df$PM2.5_real,df$PM2.5),2)
correlation_calib = round(cor(df$PM2.5_real,df$PM2.5_calib2),2)
correlation_calibRIVM = round(cor(df$PM2.5_real,df$PM2.5_calib),2)

png(filename=paste0(outdir2,"06_Correlation_real_raw_sensdata_012022_nmax1000_distmaxRepmax.png"), width=600, height=600, type="cairo",bg = "white")
par(mar=c(7,9,2,9)) # margin bot left top right (need space for the lgd
plot(df$PM2.5_real,df$PM2.5, pch=16, col="black",main="",cex.main=2, xlab=bquote(Real ~ .(pol) ~ (mu*g/m^3)),ylab=bquote(SensorRaw ~ .(pol) ~ (mu*g/m^3)),cex.lab=1.8, cex.axis=1.8)
reg=lm(PM2.5_real~PM2.5,df)
abline(reg,col="red", lwd=4)
abline(0,1,lwd=4,lty=2,col="red")
legend("bottomright",paste("R=",correlation_raw,sep =""),box.col=0,cex=1.8)
dev.off()

png(filename=paste0(outdir2,"06_Correlation_real_calib_sensdata_012022_nmax1000_distmaxRepmax_outliers.png"), width=600, height=600, type="cairo",bg = "white")
par(mar=c(7,9,2,9)) # margin bot left top right (need space for the lgd
plot(df$PM2.5_real,df$PM2.5_calib2, pch=16, col="black",main="",cex.main=2, xlab=bquote(Real ~ .(pol) ~ (mu*g/m^3)),ylab=bquote(SensorCalib ~ .(pol) ~ (mu*g/m^3)),cex.lab=1.8, cex.axis=1.8)
reg=lm(PM2.5_real~PM2.5_calib2,df)
abline(reg,col="red", lwd=4)
abline(0,1,lwd=4,lty=2,col="red")
legend("bottomright",paste("R=",correlation_calib,sep =""),box.col=0,cex=1.8)
dev.off()

png(filename=paste0(outdir2,"06_Correlation_real_calibRIVM_sensdata_012022_nmax1000_distmaxRepmax.png"), width=600, height=600, type="cairo",bg = "white")
par(mar=c(7,9,2,9)) # margin bot left top right (need space for the lgd
plot(df$PM2.5_real,df$PM2.5_calib, pch=16, col="black",main="",cex.main=2, xlab=bquote(Real ~ .(pol) ~ (mu*g/m^3)),ylab=bquote(SensorCalib ~ .(pol) ~ (mu*g/m^3)),cex.lab=1.8, cex.axis=1.8)
reg=lm(PM2.5_real~PM2.5_calib,df)
abline(reg,col="red", lwd=4)
abline(0,1,lwd=4,lty=2,col="red")
legend("bottomright",paste("R=",correlation_calibRIVM,sep =""),box.col=0,cex=1.8)
dev.off()


#Boxplots
P1 <- as.data.frame(metrics_final[,3])
names(P1)[names(P1) == "metrics_final[, 3]"] <- "VAL"
P1$METRIC <- as.vector(t(rep("MBE",length(metrics_final[,1]))))
CASE <- metrics_final[,7]
CASE=as.factor(CASE)
P1$CASE=CASE

P2 <- as.data.frame(metrics_final[,4])
names(P2)[names(P2) == "metrics_final[, 4]"] <- "VAL"
P2$METRIC <- as.vector(t(rep("MAE",length(metrics_final[,1]))))
CASE <- metrics_final[,7]
CASE=as.factor(CASE)
P2$CASE=CASE

P3 <- as.data.frame(metrics_final[,5])
names(P3)[names(P3) == "metrics_final[, 5]"] <- "VAL"
P3$METRIC <- as.vector(t(rep("RMSE",length(metrics_final[,1]))))
CASE <- metrics_final[,7]
CASE=as.factor(CASE)
P3$CASE=CASE

P4 <- as.data.frame(metrics_final[,6])
names(P4)[names(P4) == "metrics_final[, 6]"] <- "VAL"
P4$METRIC <- as.vector(t(rep("MQI",length(metrics_final[,1]))))
CASE <- metrics_final[,7]
CASE=as.factor(CASE)
P4$CASE=CASE

P5 <- as.data.frame(metrics_final[,2])
names(P5)[names(P5) == "metrics_final[, 2]"] <- "VAL"
P5$METRIC <- as.vector(t(rep("R2",length(metrics_final[,1]))))
CASE <- metrics_final[,7]
CASE=as.factor(CASE)
P5$CASE=CASE

PF <- rbind(P1,P2,P3)#,P4,P5)
#PF <- rbind(P4,P5)

PF$VAL = as.numeric(PF$VAL)

# png(filename=paste0(outdir2,"06_MBE_MAE_RMSE_sensor_calibration_012022_nmax1000_distmaxRepmax_outliers.png"), width=600, height=400, type="cairo",bg = "white")
# #png(filename=paste0(outdir2,"06_MQI_R2_sensor_calibration_012022_nmax1000_distmaxRepmax_outliers.png"), width=600, height=400, type="cairo",bg = "white")
# 
# 
# p <- ggplot(PF, aes(x=PF$METRIC, y=PF$VAL, fill=CASE)) +
#        stat_boxplot(geom ='boxplot') +
#        geom_boxplot(outlier.colour = "#1F3552", outlier.shape = 20, notch=FALSE) + #ylim(-1,5)+
#        scale_fill_brewer(palette="Set2")+
#        labs(x="", y = expression(paste("(", mu, "g/", m^3, ")")))+
#        theme_bw() +
#        theme(panel.grid.major = element_line(colour = "#d3d3d3"),
#              panel.grid.minor = element_blank(),
#              panel.border = element_blank(),
#              panel.background = element_blank(),
#              plot.title = element_text(size = 18, family = "Arial"),
#              text=element_text(family = "Arial"),
#              axis.title = element_text(size=18),
#              axis.text.x = element_text(colour="black", size = 18),
#              axis.text.y = element_text(colour="black", size = 18),
#              axis.line = element_line(size=0.5, colour = "black"),
#              legend.title = element_blank(),
#              legend.text=element_text(size=18))
# p
# dev.off()