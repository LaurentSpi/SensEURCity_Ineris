###############################
## Between sensor uncertainty ##
################################
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

#####################################
## Certification Field uncertainty ##
#####################################
UCi <- function(DataFrame, Reference, Sensor, CalModel, ubRM, LV, Corr = NA){
  ## This function return the uncertainty of measurements of the field tests as described in the draft of the sensor evaluation protocol from the CEN-TC264-WG42
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
  
  UCi.df <- DataFrame[, c(Reference,Sensor)]
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

## Set of ubRM and LV used for sensor certification:
## Define LV and ubRM for NO2
#LV_NO2 <- 200
#ubRM_NO2 <- 0.03*LV_NO2
## Define LV and ubRM for O3
#LV_O3 <- 120
#ubRM_O3 <- 0.03*LV_O3
## Define LV and ubRM for PM2.5
#LV_PM25 <- 50
#ubRM_PM25 <- 0.03*LV_PM25
## Define LV and ubRM for PM10
#LV_PM10 <- 50
#ubRM_PM10 <- 0.03*LV_PM10