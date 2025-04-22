## Set of function extracted from the AirSensEUR toolbox available here : https://github.com/ec-jrc/airsenseur-calibration
## Reference:
## The JAVA software , shell scripts, C applications, drivers and configuration files developed for the AirSensEUR sensor box
## is released under the European Public License, an open source license for software. The copyright notice is as follows: 
## License Copyright 2018 EUROPEAN UNION Licensed under the EUPL, Version 1.2 or subsequent versions of the EUPL (the "License"); 
## You may not use this work except in compliance with the License. A copy of the License is given here after. 
## Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on 
## an "AS IS" basis, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific 
## language governing permissions and limitations under the License https://eupl.eu/1.2/en/. 

## Authors:
## Michel Gerboles, michel.gerboles@ec.europa.eu 
## Laurent Spinelle, laurent.spinelle@ineris.fr 
## Alexander Kotsev, alexander.kotsev@ec.europa.eu 
## Federico Karagulian, federico.karagulian@ec.europa.eu 
## Marco Signorini, marco.signorini@liberaintentio.com

################################
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



###############################################################################
## Function radarchart (20181120)
###############################################################################
## Modification of the radarchart form the fmsb package to add the color for the axis name
## Package fmsb version 0.6.3

RadarChartLSp <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                           plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                           cglwd = 1, cglcol = "navy", axislabcol = "blue", title = "", 
                           maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, vlabels = NULL, 
                           vlcex = NULL, caxislabels = NULL, calcex = NULL, paxislabels = NULL, 
                           palcex = NULL, ...){
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
       axes = FALSE, xlab = "", ylab = "", main = title, asp = 1, 
       ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3) 
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5) 
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels))) 
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | 
        axistype == 5) {
      if (is.null(calcex)) 
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
             col = axislabcol)
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                col = axislabcol, cex = calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels)) 
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex)) 
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
              cex = palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) 
    VLABELS <- vlabels
  if (is.null(vlcex)) 
    text(xx * 1.2, yy * 1.2, VLABELS, col = axislabcol)
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, col = axislabcol)
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
    ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, 
                  df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 
                              1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) + 
              xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) + 
              yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) + 
              xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            yyright <- yy[right] * CGap/(seg + CGap) + 
              yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + 
                                                                                            CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                 xxleft)/(yy[j] * (xxright - xxleft) - 
                                            xx[j] * (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, 
                                                 j]) * seg/(seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                         2])
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2], 
             col = pcols[i - 2])
    }
  }
}

###############################################################################
### Function mape (20181002)
##############################################################################
mape <- function(actual,pred){
  
  ## Based on https://stats.stackexchange.com/questions/65421/performance-evaluation-of-a-model-mape-in-r/65443
  ## This function calculate the mean absolute percentage error.
  ## It returns the MAPE value, the number of Inf values and the percentage of data it represent.
  mape_perc <- mean(abs((actual - pred)/actual))*100
  if(!is.finite(mape_perc)){
    mape_perc <- abs((actual - pred)/actual)
    inf.data <- length(subset(mape_perc, !is.finite(mape_perc)))
    inf.perc <- round(inf.data/length(mape_perc)*100, digits=2)
    if(inf.perc > 10){
      mape_perc <- NA
    }else{
      mape_perc <- subset(mape_perc, is.finite(mape_perc))
      mape_perc <- mean(mape_perc)*100
    }
  }else{
    inf.data <- NA
    inf.perc <- NA
  }
  return (cbind(mape_perc, inf.data, inf.perc))
}

###############################################################################
### Function Load.Packages (170420)
##############################################################################
Load.Packages <- function(list.Packages) {
  # list.Packages                 vector of names of the packages to load
  # 
  cat("", sep = "\n")
  cat("[Load.Packages] INFO CHECK Installed packages and Toolbox to run the script", sep = "\n")
  #
  for(i in list.Packages) {
    
    # Installing packages
    if(i %in% rownames(installed.packages()) == FALSE) {
      cat(sprintf("[Load.Packages] INFO Installing %s", i), sep = "\n")
      install.packages(i)
    } else {
      cat(sprintf("[Load.Packages] INFO Package %s already installed",i), sep = "\n")
    }
    # cat(i,quote=FALSE)
    do.call("library", as.list(i))
    #library(i, character.only = TRUE)
    cat(sprintf("[Load.Packages] INFO Package %s loaded",i), sep = "\n")
    
  }
  #
  # List of loaded packages
  cat("[Load.Packages] INFO List of installed packages", sep = "\n")
  print(search(), quote = FALSE)
  cat("", sep = "\n")
  
}

##############################################################################
### resetPar: Function reset graphical parameters (Vs 141114)
##############################################################################
resetPar <- function() {
  # http://stackoverflow.com/questions/5789982/reset-par-to-the-default-values-at-startup
  # reset par by par(resetPar())
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

##############################################################################
### sink.reset: Function reset sink number (Vs 141114)
##############################################################################
sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}

##############################################################################
### stopWhenError: Function reset sink and device errors (Vs 141114)
##############################################################################
stopWhenError <- function(FUN) {
  # stopWhenError(sink) # for sink.
  # stopWhenError(dev.off) # close all open plotting devices.
  tryCatch({
    while(TRUE) {
      FUN()
    }
  }, warning = function(w) {
    print("All finished!")
  }, error = function(e) {
    print("All finished!")
  })
}

############################################
### slope_orth: Function Orthogonal regression (Vs 141114)
############################################
slope_orth <- function(Xlabel, Ylabel, Title, DQO = NULL, LV = NULL, Units = NULL, Disk = NA, WD = NA, Dir = NA, Mat, uxi = NULL, lim, Sensor_name = NULL, variable.uxi = FALSE, 
                       f_coef1 = NULL, f_coef2 = NULL, f_R2 = NULL) {
  # return a data matrix with the orthogonal regression: b, ub, a, ua, RSS, RMSE, MBE, Correlation, nb, CRMSE, NMSD, Mat, sx, sy 
  # Xlabel, Ylabel : label On the x And y axis
  # Title          : title to appear On the top of the scatter plot of x And y values
  # DQO            : numeric, data qualtiy objective for the expanded uncertainty, sam unit as Mat$yis, defaul NULL. If NULL nod DQO, horiztial line is plotted
  # LV             : numeric, limit value for Mat$xis, same unit as Mat$xis, default value = NULL, plot a vertical line at LV if not NULL
  # Units          : character vector, units for the expanded uncertainty, Xis, Yis
  # Disk, WD, Dir  : where you put the graphic files (Disk, working directory, directory), It is sufficient if only Dir is supplied
  # lim            : passing variable for the limits of the Etalonnage function (cbind(c(minX,maxX),c(minY,maxY)) or NULL)
  # Sensor_name    : name of the sensor to be written in front of the calibration equation. If NULL, do not print sensor name.
  # Mat            : DataFrame of data including Case number, Date, x And y + optional uxi if uxi is not constant for all reference values
  # uxi            : numeric (default = NULL), random standard uncertainty of the results of the reference method, xis, given as a constant value for all xis reference values
  # variable.uxi   : logical, if FALSE (default = FALSE ), uxi is used as constant random standard uncertainties for all xis reference values. 
  #                  If TRUE uxi given in Mat is used for each reference values
  # f_coef1, f_coef2, f_R2: number of digit for intercept, slope and R2 using sprintf syntax. 
  #                         f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
  
  # 
  # Homogeneity of variance is tested For the calculation of RSS
  # adding Ur In a New field of Mat
  # returning a list with slope (b and ub), intercept (a and ua), the sum of square of residuals (RSS), 
  # the root means square of error (RMSE), the mean bias error (MBE), the coefficeint of correlation (Correlation), 
  # the number of valid measurment (nb), the centred rout mean square of error (CRMSE), the normalised mean standard deviation (NMSD)
  # and Mat with relative expanded uncertainty
  
  #checking that the mat dataFrame is not empty
  if(nrow(Mat)>0){
    # Creating the Directory
    Dir <- file.path(c(Disk, WD , Dir)[which(!sapply(list(c(Disk, WD , Dir)), is.na))])
    
    # saving the original par values
    op <- par(no.readonly = TRUE)
    par(mar=c(4,4,2,0.5))
    par(new=FALSE)
    
    #print(Mat)
    if(variable.uxi){
      colnames(Mat) <- c("case", "Date", "xis", "yis","uxi")
    }  else {
      colnames(Mat) <- c("case", "Date", "xis", "yis")
      # Adding uxi as constant values
      if(!is.null(uxi)) Mat$uxi <- rep(uxi, nrow(Mat))
    }
    # Filtering for the validation data only
    Mat <- subset(Mat, !is.na(Mat$xis) & !is.na(Mat$yis))
    
    #Orthogonal regression (see annex b of equivalence method)
    nb <- nrow(Mat)
    xm <- mean(Mat$xis)
    ym <- mean(Mat$yis)
    Sxx <- sum((Mat$xis - xm)^2)
    Syy <- sum((Mat$yis - ym)^2)
    Sxy <- sum((Mat$xis - xm) * (Mat$yis - ym))
    pente <- (Syy - Sxx + sqrt((Syy- Sxx)^2 + 4*Sxy^2))/(2*Sxy)
    ordon <- ym - pente * xm
    upente <- sqrt((Syy - (Sxy^2/Sxx))/((nb-2)*Sxx))
    uordon <- sqrt(upente^2 * sum(Mat$xis^2)/nb)
    RSS <- sum((Mat$yis - (ordon + pente * Mat$xis))^2)
    sx <- sd(Mat$xis)
    sy <- sd(Mat$yis)
    
    # Regression statistics for Target Diagram (see delta tool user guide)
    RMSE <- sqrt((sum((Mat$yis - (ordon + pente * Mat$xis))^2))/nb)
    MBE <- 1/nb * sum(Mat$yis - Mat$xis)
    CRMSE <- sqrt(1/nb * sum(((Mat$yis - ym) - (Mat$xis - xm))^2))
    NMSD <- (sd(Mat$yis) - sd(Mat$xis)) / sd(Mat$xis)
    Correlation <- cor(Mat$xis,Mat$yis)
    
    #Plots scatterplot
    Gamme <- Etalonnage(x=Mat$xis, s_x=NULL, y=Mat$yis, s_y=NULL, 
                        AxisLabelX=Xlabel, AxisLabelY=Ylabel, Title=Title , Marker = 19, Couleur = 'blue', lim = NULL, XY_same= TRUE, digitround = NULL, marges=NULL, PlotAxis = "s")
    #Cal_Line(x = Mat$xis, s_x = NULL, y = Mat$yis, s_y = NULL, Mod_type = 'Linear', Matrice = Mat, line_position = 0, Couleur = 'red', Sensor_name, f_coef1 = "%.3e", f_coef2 = , "%.3e", f_R2 = "%.4f", lim = gamme) # line of the linear regression
    par(new=TRUE)
    lines(Mat$xis, pente * Mat$xis + ordon, type = "l", col = "red", xlim = Gamme[,1], ylim = Gamme[,2])
    if(is.null(f_coef1)) f_coef1 <- "%.2f"
    if(is.null(f_coef2)) f_coef2 <- "%.2f"
    if(is.null(f_R2))    f_R2    <- "%.4f"
    mtext(sprintf(paste0(Sensor_name, ", y= ",f_coef1,"+ ",f_coef2," x",", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1),
                  ordon, pente, cor(Mat$xis,Mat$yis)^2, sd(pente * Mat$xis + ordon - Mat$yis), 
                  sqrt(sum((pente * Mat$xis + ordon - Mat$yis)^2)/(length(pente * Mat$xis + ordon - Mat$yis)-2))
                  ,line=0,adj=1,padj=0,col="Red",cex=0.875))
    #mtext(sprintf("y = %.1f + %.2f x, R2 = %.5f", ordon, pente, cor(Mat$xis,Mat$yis)^2), line=0, col ="Red")
    dev.copy(png,filename = file.path(Dir,sprintf("%s_Scatter.png", Title)), units = "cm", res = 300, width = 22, height = 22);
    dev.off()
    
    #### Calculating uncertainty
    Mat$bias <- (ordon+(pente-1)*Mat$xis)
    # Plots square of absolute residuals versus xis
    if(!is.null(Units)) Ylab = paste0("Square of Residuals in (", Units,")?") else Ylab="Square of Residuals"
    gamme <- Etalonnage(x=Mat$xis, s_x=NULL, y=(Mat$yis - (ordon + pente * Mat$xis))^2, s_y=NULL, Xlabel,
                        AxisLabelY = Ylab, Title=Title , Marker=19, Couleur='blue', lim = NULL, XY_same= FALSE, digitround=NULL, marges=NULL, PlotAxis = "s")
    #function(x, s_x, y, s_y, AxisLabelX, AxisLabelY, Title, Marker, Couleur, ligne=NULL, XY_same, lim, steps = c(10,10), digitround = NULL, marges = NULL, PlotAxis = "n")
    Cal_Line(x = Mat$xis, s_x = NULL, y = (Mat$yis - (ordon + pente * Mat$xis))^2, s_y = NULL, 
             Mod_type = 'Linear', Matrice = Mat, line_position = 0, Couleur = 'red', Sensor_name, f_coef1 = "%.3e", f_coef2 = , "%.3e", f_R2 = "%.4f", lim = gamme) # line of the linear regression
    # browser()
    if(!dir.exists(Dir)) dir.create(Dir, showWarning = TRUE, recursive = TRUE)
    dev.copy(png,filename = file.path(Dir,sprintf("%s_SqrRes.png", Title)), units = "cm", res = 300, width = 22, height = 22);
    dev.off()
    
    # tesing significance of correlation between s and square of absolute reisduals - The calculation does not work only possibility the constrant RSS
    rtest <- cor.test(Mat$xis,(Mat$yis - (ordon + pente * Mat$xis))^2)
    print(rtest)
    print(sprintf("probability of H0 (r=0): %f, if <0.05, correlation is demonstrated, if > 0.95 there is no correlation",rtest$p.value),quote=FALSE)
    if(rtest$p.value < 0.00) { ############################################################## should be < 0.05
      cat("The residuals are not constant. RSS is calculated with equation for constant relative residuals.\n")
      print(sprintf("square root of mean sum of squared relative Residuals: %f", sqrt(sum((Mat$yis/(ordon+pente*Mat$xis)-1)^2))/nb^2), quote = FALSE)
      # Mat$RSS <- (ordon + pente * Mat$xis)^2 * sum((Mat$yis/(ordon+pente*Mat$xis)-1)^2 )
      z <- lm((Mat$yis - (ordon + pente * Mat$xis))^2 ~ Mat$xis)
      Mat$RSS <- fitted(z)
      Mat$Ur <- 2 * sqrt(Mat$RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
      Mat$U  <- 2 * sqrt(Mat$RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)
    } else 
    {
      cat("The residuals are constant. RSS is calculated with equation for constant residuals.\n")
      if(any((RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2) < 0)){
        cat(" Some RSS/(nb-2) - uxi^2 + bias^2 are negative. The measurement uncertainty cannot be calculated. Decrease uxi, see below x and max(uxi). Returning NAs for Ur and U.\n")
        print(data.frame(x=Mat$xis, uxi = Mat$uxi, Max.uxi=sqrt(RSS/(nb-2) + Mat$bias^2), Max.RSD = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis, 
                         Decrease.uxi = (Mat$uxi - sqrt(RSS/(nb-2) + Mat$bias^2)) > 0))
        Mat$Ur <- NA
        Mat$U  <- NA            
      }  else {
        cat("RSS/(nb-2) - uxi^2 + bias^2  are positive. The measurement uncertainty can be calculated, see below x and max(uxi).\n")
        print(data.frame(x=Mat$xis, uxi = Mat$uxi, Max.uxi=sqrt(RSS/(nb-2) + Mat$bias^2), Max.RSD = sqrt(RSS/(nb-2) + Mat$bias^2)/Mat$xis))
        RSS    <- sum((Mat$yis - (ordon + pente * Mat$xis))^2)
        Mat$Ur <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)/Mat$xis * 100
        Mat$U  <- 2 * sqrt(RSS/(nb-2) - Mat$uxi^2 + Mat$bias^2)  
        
        #Plots Ur
        # Etalonnage(Mat$xis, NULL, Mat$Ur, NULL, Xlabel,
        #            "Relative expanded uncertainty in %", Title , 19, 'blue', lim = NULL, XY_same= FALSE, digitround=c(0,0), marges=NULL, PlotAxis = "s") # not used
        order.xis <- order(Mat$xis)
        if(!is.null(Units)) Ylab = paste0("Expanded uncertainty in ", Units) else Ylab="Expanded uncertainty"
        plot(Mat[order.xis, "xis"], Mat[order.xis, "U"], xlab=Xlabel, ylab = Ylab, main=Title , col='blue', type = "l", ylim= c(0, max(Mat$U, na.rm = T)))
        
        # DQO = 0.30*1.5 # in ppb
        # LV  = 1.5
        if(!is.null(LV)) {
          abline(v=LV)
          text(LV, 0, "LV")  
        } 
        if(!is.null(DQO)) {
          abline(h=DQO) # in ppb
          text(0, DQO, "DQO")  
        } 
        # UAT = 0.35 /1.91 * 78/46
        # LAT = 0.25 /1.91 * 78/46
        # lines(Mat[Mat$xis> LAT, "xis"], Mat[Mat$xis> LAT, "xis"]*DQO, col="black", lwd=3)
        # lines(Mat[Mat$xis< LAT, "xis"], rep(LAT*DQO, length.out = length(Mat[Mat$xis< LAT, "xis"])), col="black", lwd=3)
        # text(5, LAT*DQO+2, "Class 1")
        # DQO = 0.75
        # lines(Mat[Mat$xis> UAT, "xis"], Mat[Mat$xis> UAT, "xis"]*DQO, col="black")
        # lines(Mat[Mat$xis< UAT, "xis"], rep(UAT*DQO, length.out = length(Mat[Mat$xis< UAT, "xis"])), col="black")
        # text(5, UAT*DQO+2, "Class 2")
        grid (NULL,NULL, lty = 2, col = "grey")
        dev.copy(png,filename = file.path(Dir,sprintf("%s_U.png", Title)), units = "cm", res = 300, width = 22, height = 22);
        dev.off()
      }
      
    }
    
    # Printing
    cat("\n")
    cat("--------------------------------\n")
    print(sprintf("Slope: %.4g +/- %.4g",pente,upente),quote=FALSE)
    print(sprintf("Intercept: %.4g +/- %.4g",ordon,uordon),quote=FALSE)
    print(sprintf("R2: %.4g",Correlation^2),quote=FALSE)
    if (rtest$p.value < 0.00) { ########################################################### should be < 0.05
      cat("The residuals are not constant. RSS is calculated with equation for constant relative residuals. The means RSS in range value is:\n")
      RSS <- mean(RSS,na.rm = TRUE)
      print(sprintf("RSS: %.4g ",RSS),quote=FALSE)
    } else 
    {
      print("The residuals are constant. RSS is calculated with equation for constant residuals:")
      print(sprintf("RSS: %.4g ",RSS),quote=FALSE)
    }
    print(sprintf("RMSE: %.4g ",RMSE),quote=FALSE)
    print(sprintf("MBE: %.4g ",MBE),quote=FALSE)
    print(sprintf("CRMSE: %.4g ",CRMSE),quote=FALSE)
    print(sprintf("NMSD: %.4g ",NMSD),quote=FALSE)
    print(sprintf("n: %.4g ",nb),quote=FALSE)
    
    
    calib <- list(pente, upente, ordon, uordon, RSS, RMSE, MBE, Correlation, nb, CRMSE, NMSD, Mat, sx, sy)
    
    # resuming the par values
    on.exit(par(mar=op))
    
  } else{
    print("The Mat dataFrame is empty. Returning NAs.")
    calib <- list(pente=NA, upente=NA, ordon=NA, uordon=NA, RSS=NA, RMSE=NA, MBE=NA, Correlation=NA, nb=NA, CRMSE=NA, NMSD=NA, Mat=NA, sx=NA, sy=NA)
  } 
  
  return(calib)
}

##############################################################################
### Etalonnage: Function View Scatter Plot of calibration function (Vs 170420)
##############################################################################
Etalonnage <- function(x, s_x, y, s_y, AxisLabelX, AxisLabelY, Title, Marker, Couleur, ligne=NULL, XY_same = FALSE, lim = NULL, steps = c(10,10)
                       , digitround = NULL, marges = NULL, PlotAxis = "s", OrdonneeOrigine = NULL) {
  # This function plot a typical XY calibration graph, estimate Xlim and ylim, add x and y labels and gridlines
  # Title 
  # Marker: type marker, typical 19
  # Couleur: color of the marker, ex 'blue'
  #### NEW ###ligne: used in plot as type = 'lign' to have dots or lines or ...
  # XY_same: if TRUE: X and Y have the same extent, IF False: Xlim ranges between min(x) and max(x) and ylim ranges between min(y) and max(y)
  # lim : if NULL Xlim and Ylim are calculated otherwise lim is used. Lim must be a matrix of xlim and ylim in column vector
  # steps: number of steps on the x an y axis, if NULL, default =10 as c(stepX, stepY)
  # digitround: number of digit for x and y axis labels (c(digitX,digitY)), should be c(0,0) for one digit
  # marges: margin of graph, default c(4,4,3,0.5)) if NULL
  # PlotAxis: = "n" to disable the plot of the axis. If empty the axis will be plot
  # for consistency with previous version of function Etalonnage if ligne is not given
  # OrdonneeOrigine: minimum value for the x an y axis as c(Xmin,Ymin), if NULL, default = NULL gives automatic min
  
  if(is.null(ligne)) ligne = "p"
  
  # saving the original par values
  op <- par(no.readonly = TRUE)
  
  # settings the margins
  if(is.null(s_y) || any(s_y == 0)) {par(mar = c(4,4,3,0.5))} else {par(mar = marges)}  
  
  # Creating the DataXY data frame
  if(is.null(s_y)  || any(s_y == 0)) {
    DataXY <- data.frame(cbind(x, y),stringsAsFactors = FALSE)
    colnames(DataXY) <- c("x", "y")
    #DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y))
  } else {
    DataXY <- data.frame(cbind(x, y, s_y),stringsAsFactors = FALSE)
    colnames(DataXY) <- c("x","y","s_y")
    #DataXY <- subset(DataXY, !is.na(DataXY$x) & !is.na(DataXY$y) & !is.na(DataXY$s_y))
  }  
  
  # Automatic estimation of digitround
  if(is.null(digitround)){
    Int <- c("x","y")
    Range <- sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[2,] - sapply(DataXY[,Int], range, na.rm = TRUE, finite = TRUE)[1,]
    print("Range of values for x and y", quote = FALSE)
    print(Range, quote = FALSE)
    digitround <- round(log10(1/Range))+2 # +1 gives too many digits? no it is fine
  }
  
  # Calculating the limits of the graph
  if(is.null(lim)) {
    if (isTRUE(XY_same)) {
      if(is.null(s_y) || any(s_y == 0)) {
        Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"], na.rm=TRUE),digits=digitround[1])
                  , round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"], na.rm=TRUE),digits=digitround[1]))
        Ylim <- Xlim
      } else {
        Xlim <- c(round(min(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] - DataXY$s_y, na.rm=TRUE),digits=digitround[1])
                  , round(max(DataXY[is.finite(DataXY$x),"x"], DataXY[is.finite(DataXY$y),"y"] + DataXY$s_y, na.rm=TRUE),digits=digitround[1]))
        Ylim <- Xlim
      }
    } else {
      if(is.null(s_y)|| any(s_y == 0)) {
        Ylim <- c(round(min(DataXY$y, na.rm = TRUE),digits=digitround[2])
                  , round(max(DataXY[is.finite(DataXY$y),"y"], na.rm=TRUE),digits=digitround[2]))
      } else {
        #if((max(DataXY$y + DataXY$s_y)-min(DataXY$y - DataXY$s_y)) < 1) {
        #  Ylim <- c((min(DataXY$y - DataXY$s_y)),(max(DataXY$y + DataXY$s_y)))
        #} else {
        Ylim <- c(round(min(DataXY$y - DataXY$s_y, na.rm=TRUE),digits=digitround[2])
                  , round(max(DataXY$y + DataXY$s_y, na.rm=TRUE),digits=digitround[2]))
        #}
      }
      if(class(DataXY$x)=="POSIXct") {
        Xlim <- c(min(DataXY$x),max(DataXY$x))
      } else {
        Xlim <- c(round(min(DataXY$x, na.rm=TRUE),digits=digitround[1])
                  , round(max(DataXY$x, na.rm=TRUE),digits=digitround[1]))
      }
    }
  } else {
    Xlim <- c(round(min(lim[,1]),digits=digitround[1]),round(max(lim[,1]),digits=digitround[1]))
    Ylim <- c(round(min(lim[,2]),digits=digitround[2]),round(max(lim[,2]),digits=digitround[2]))
  }
  
  if(!is.null(OrdonneeOrigine)){
    Xlim <- c(OrdonneeOrigine[1],Xlim[2])
    Ylim <- c(OrdonneeOrigine[2],Ylim[2])
  }
  
  # specifying ticks and grid
  if(is.null(steps)) {
    stepsX <- 10
    stepsY <- 10
  } else {
    stepsX <- steps[1] # was steps[,1] but imply to enter steps values as a matrix
    stepsY <- steps[2] # was steps[,2] but imply to enter steps values as a matrix
  }
  # 
  
  # plotting the scatterplot
  plot( DataXY$x, DataXY$y
        ,xlab= AxisLabelX
        ,ylab= AxisLabelY
        ,xlim = Xlim
        ,ylim = Ylim
        ,col = Couleur
        ,type = ligne
        ,pch = Marker
        ,xaxp = c(min(Xlim), max(Xlim), stepsX)
        ,yaxp = c(min(Ylim), max(Ylim), stepsY)
        ,xaxt = PlotAxis
        ,yaxt = PlotAxis
        ,cex.lab=1.25)
  
  if(!is.null(s_y) && all(s_y != 0)) {
    # hack: we draw arrows but with flat "arrowheads"
    arrows(DataXY$x, DataXY$y - DataXY$s_y , DataXY$x, DataXY$y + DataXY$s_y, length=0.05, angle=90, code=3)
  } 
  
  par(xaxp = c(min(Xlim), max(Xlim), stepsX), yaxp = c(min(Ylim), max(Ylim), stepsY))
  grid (NULL,NULL, lty = 2, col = "grey")
  title (main = Title, outer = TRUE, line = -1)
  
  # Passing and resuming the par values
  np <- par(no.readonly = TRUE) # to be returned
  #on.exit(par(op))
  
  #return(cbind(Xlim,Ylim))
  return(cbind(Xlim,Ylim, par("usr")[1:2], par("usr")[3:4])) # par("usr") gives the true chosen xlim and ylim to which 4% of range is added in both sides. Used for arrows
}

##############################################################################
### f_log: Function Fitting a logarithmic model (Vs 141114)
##############################################################################
f_log <- function(x, a, b) {
  return(y = a + b * log(x))
  # Increases without bound to right
  # Passes through (1,a),
  # Very rapid growth, followed by slower growth,
  # Common log will grow slower than natural log
  # b controls the rate of growth
}

##############################################################################
### f_Unitec: Function Fitting a the Unitec model (Vs 141114)
##############################################################################
f_Unitec <- function(x, a, b,c) {
  # mVolt <- ((y-a)/b)^(1/c) 
  return(y = (((x*1.91*(293.15/TK))-a)/b)^(1/c))
  # Y(?g/m3) (*1.91) = a+b*X(mV)^c 
  # with a=-31.6  b=5330.9  -0.598, x en mV	min(x)=47.2	max(x)=5299.9	unitY=?g/m3	ymax=500?g/m3
  # Equation of Unitec
}

##############################################################################
### f_ExpDI: Function Fitting an exponential Decay (Increasing form) model (Vs 141114)
##############################################################################
f_ExpDI <- function(x, C, k) {
  # C is the asymtoptic
  # data shall pass by 0,0
  return(C *(1-exp(-k*x)))
}

##############################################################################
### f_ExpDI_Int: Function Fitting an exponential Decay (Increasing form) model with intercept (see wikipedia) (Vs 141114)
##############################################################################
f_ExpDI_Int <- function(x, C, k,intercept) {
  # C is the max asymtoptic value on y axis
  # intercept is the min asymtoptic value on y axis
  return(C *(1-exp(-k*x)) + intercept)
}

##############################################################################
### f_Michelis: Function Fitting a Michaelis-Menten kinetics model with intercept (Vs 141114)
##############################################################################
f_Michelis <- function(x, Vmax, km, intercept) {
  # Vmax is the max asymtoptic value on y axis
  # intercept is the min asymtoptic value on y axis
  # km is The Michaelis constant is the concentration at which the sensor response is half of V_\max
  return(Vmax*x/(km +x) + intercept)
}

##############################################################################
### f_ExpDD_Int: Function Fitting an exponential Decay (Decreasing form) model with intercept (Vs 141114)
##############################################################################
f_ExpDD_Int <- function(x, C, k,intercept) {
  # C is the max value on y axis at x= 0
  # intercept is the min asymtoptic value on y axis
  return(C *(exp(-k*x)) + intercept)
}

##############################################################################
### f_Sigmoid: Function Fitting a Logistic - Sigmoidal function (Vs 141114)
##############################################################################
f_Sigmoid <- function(x, MIN, Asym, xmid, Hill) {
  return(y = MIN + (Asym-MIN) / (1 + (xmid/x)^Hill))
  #, Hill > 0, between 1a dn 10
  #Asymptotic to y = Max to right,
  #Asymptotic to y = Min to left,
  #Passes through (0, a/(1+b) )
  #x50 x value at the inflection point, in this equation x is not log transformed
}

##############################################################################
### Estimated.y: Function Plot estimated function (Vs 141114)  
##############################################################################
Estimated.y <- function(x, Model) {
  # This function estimates the y values for at 1000 points of x from min(x) until max(x)
  # Return : data.fram estimated with variable x and y, each one with 5000 points ina x ascending order
  # x: Data used to establish the model (only x)
  # Model: the Model to plot
  
  Estimated <- data.frame(x = seq(min(x),max(x),length.out=5000), y= seq(min(x),max(x),length.out=5000))
  
  if(inherits(Model, "gam")){ # checking if the model was fitted from gam function
    Estimated$y <- predict.gam(Model, newdata = Estimated, type = "response")
  }
  else{
    Estimated$y <- predict(Model, newdata = Estimated)  
  } 
  
  return(Estimated)
} 

##############################################################################
### Cal_Line: Function calibration function and plot calibration line (VS 170428)
##############################################################################
Cal_Line <- function(x, s_x, y, s_y, Mod_type,  Matrice=NULL, line_position, Couleur, Sensor_name = NULL, f_coef1, f_coef2, f_R2
                     , lim = NULL, marges = NULL, Covariates = NULL, Equation = NULL, ubRM = NULL, LV = NULL) {
  # This Function estimates the calibration function, plots the calibration line and write the equation above the plot at line_position
  # It also returns the estimated model  
  # The regression equation can be weithed (1/sy^2) or not if s_y = Null
  # x, s_x, y, s_y: x and y values with their standard devation that can be equal to Null
  # Mod_type: type of calibration model: Linear, ...
  # Mod_type = "Zero" is used to force the linear regression through the origin as described here https://rpubs.com/aaronsc32/regression-through-the-origin
  # Matrice: Input Matrix of data (e. g. Pre_cal_df) ###################### This parameter was deleted
  # line position: for mtext of the regression equation
  # Couleur: color of the line and color font of the equation
  # Sensor_name: name of the sensor to be written in front of the calibration equation. If NULL, sensor name is not printed
  # f_coef1, f_coef2, f_R2: number of digit for intercept, slope and R2 using sprintf syntax. 
  #                         f_coef1 is used both for intercept and s(Res), f_coef2 is used for all parameters of the model apart from the intercept
  # lim: matrix of column vectors Xlim, YLIM: limits of the plots where to add the calibration line. When NULL the lim are automatically calculated.
  # marges: margin of graph, if NULL set as c(4,4,3,0.5)). If you don't want to set the margin, write "NO"
  # Equation: type of info to be printed on the graph for the Linear model, RMSE correspond to the full set,
  #            "MAPE" show the MAPE coefficient and "Simple" show only ax+b and R^2, "Uncertainty" will display the uncertainty+RMSE+equation+R2
  # ubRM: betwem reference measurment device uncertainty, shall be given if the Equation variable is set to "Uncertainty".
  # LV: selected limit value for the uncertainty estimation, shall be given if the Equation variable is set to "Uncertainty".
  
  # saving the original par() values
  op <- par(no.readonly = TRUE)
  # resuming the par values
  on.exit(par(op))
  
  # settings the margins
  if(is.null(marges)){
    Margin <- c(4,4,3,0.5)
    par(mar = c(4,4,3,0.5))	
  } else {
    Margin <- par("mar")
  }
  
  #Define the limits of the graphs
  if(is.null(lim)){
    Xrange <- c(min(x), max(x))
    Yrange <- c(min(y), max(y))
    lim = cbind(Xrange, Yrange)
  }
  
  #Put O3, sensor reponses and standard deviation of sensor responses in a matrix remove NA of x and y
  if(is.null(s_y) || any(s_y == 0)) {
    DataXY <- data.frame(cbind(x, y))
    # removing NA of s_y
    DataXY <- subset(DataXY, !is.na(x) & !is.na(y))
    colnames(DataXY) <- c("x","y")
  } else {
    DataXY <- data.frame(cbind(x, y, s_y))
    # removing NA of s_y
    DataXY <- subset(DataXY, !is.na(x) & !is.na(y) & !is.na(s_y) & !s_y==0)
    DataXY$wi <- DataXY$s_y^-2/sum(DataXY$s_y^-2)
    colnames(DataXY) <- c("x","y","s_y","wi")
  }
  
  # Add ", " at the end of Sensor_name to be print with the legend
  if(!is.null(Sensor_name)){
    Sensor_name <- paste0(Sensor_name, ", ")
  }
  
  # Linear Model, if s_y is not null calculate weights wi and use them in the regression
  if(Mod_type=='Linear') {
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- lm(y ~ x, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- lm(y ~ x, data = DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    if(is.null(marges)){
      par(new=TRUE)
    } else {
      par(new=TRUE)#, mar=Margin)
    }
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # print full set of parameters with ax+b, R^2, s(Res), RMSE and AIC
    if(Equation == "RMSE"){
      if(coef(Model)[1]>=0){
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x+",f_coef1,", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1,",AIC= %.1f ")
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)) ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.2)
      } else {
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x",f_coef1,", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1,",AIC= %.1f ")
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),
                      AIC(Model)) ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.2)
      }
    }
    
    # print parameters with ax+b, R^2, s(Res) and MAPE
    if(Equation == "MAPE"){
      if(coef(Model)[1]>=0){
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x+",f_coef1,", R2=",f_R2,", s(Res)=",f_coef1,", MAPE=",f_coef1)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sd(resid(Model)), mape(actual=Model$x[,2], pred=Model$fitted.values)[1])
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      } else {
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x",f_coef1,", R2=",f_R2,", s(Res)=",f_coef1,", MAPE=",f_coef1)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sd(resid(Model)), mape(actual=Model$x[,2], pred=Model$fitted.values)[1])
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      }
    }
    
    # print parameters with ax+b, R^2, RMSE and Uncertainty
    if(Equation == "Uncertainty"){
      if(coef(Model)[1]>=0){
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x+",f_coef1,", R2=",f_R2,", RMSE=",f_coef1,", UCi=",f_coef1)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sqrt(sum(resid(Model)^2)/(length(resid(Model))-2))
                      , UCi(DataXY, DataXY$x, DataXY$x, c(coef(Model)[1], coef(Model)[2]), ubRM, LV, Corr = NA))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      }
      
      else {
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x",f_coef1,", R2=",f_R2,", RMSE=",f_coef1,", UCi=",f_coef1)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared,sqrt(sum(resid(Model)^2)/(length(resid(Model))-2))
                      , UCi(DataXY, DataXY$x, DataXY$x, c(coef(Model)[1], coef(Model)[2]), ubRM, LV, Corr = NA))
              ,line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      }
    }
    
    # print only simple parameters with ax+b and R^2
    if(Equation == "Simple"){
      if(coef(Model)[1]>=0){
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x+",f_coef1,", R2=",f_R2)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared),line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      } else {
        mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef2,"x",f_coef1,", R2=",f_R2)
                      , coef(Model)[2], coef(Model)[1], summary(Model)$r.squared),line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
      }
    }
  }
  
  if(Mod_type=='Zero') { ## source: https://rpubs.com/aaronsc32/regression-through-the-origin 
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- lm(y ~ 0+x, data = DataXY, model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- lm(y ~ 0+x, data = DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibration lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    if(is.null(marges)){
      par(new=TRUE)
    } else {
      par(new=TRUE)#, mar=Margin)
    }
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # If Zero is selected in Mod_Type, only the Simple parameters with ax and R^2 are printed
    mtext(sprintf(paste0(Sensor_name, "Linear: y= ",f_coef1,"x",", R2=",f_R2)
                  , coef(Model)[1], summary(Model)$r.squared),line=line_position,adj=1,padj=0,col=Couleur,cex=1.4)
  }
  
  # MGV Robust Linear Model, (if s_y is not null calculate weights wi and use them in the regression
  # This models the median of y as a function of x, rather than modelling the mean of y as a function of x, in the case of least squares regression.
  if(Mod_type=='Linear.Robust') {
    if("quantreg" %in% rownames(installed.packages()) == FALSE) {install.packages("quantreg")}
    library("quantreg")
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- rq(y ~ x, data = DataXY, tau=0.5, model = TRUE)
    } else {
      Model <- rq(y ~ x, data = DataXY, weights = wi, tau=0.5, model = TRUE)
    }
    print(summary(Model))
    
    # plotting calibration lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    par(bg="blue")
    #mtext(sprintf(paste0("Robust Linear: y= ",f_coef1,"+ ",f_coef2," x, Std.error= ",f_R2,", t value= ",f_coef1),coef(Model)[1], coef(Model)[2]
    #              , summary(Model)$r.squared,sd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    #             , summary(Model)$Std.error,summary(Model)$t valuesd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    mtext(sprintf(paste0("Robust Linear: y= ",f_coef1,"+ ",f_coef2," x"),coef(Model)[1], coef(Model)[2])
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    
  }
  if(Mod_type=='gam') {
    #     if("quantreg" %in% rownames(installed.packages()) == FALSE) {install.packages("quantreg")}
    #     library("quantreg")
    Estimated <- data.frame(x=x, y=y)
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- gam(y~s(x, k=5), family=Gamma(link=log), data = Estimated) 
    } else {
      Model <- gam(y~s(x, k=5), family=Gamma(link=log), weights = wi) 
    }
    print(summary(Model))
    
    # plotting calibration lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model)
    Estimated <- Estimated[order(Estimated$x),]
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    #plot(x,Model$fitted.values, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "")
    
    # display equations and R^2
    par(bg="blue")
    #mtext(sprintf(paste0("Robust Linear: y= ",f_coef1,"+ ",f_coef2," x, Std.error= ",f_R2,", t value= ",f_coef1),coef(Model)[1], coef(Model)[2]
    #              , summary(Model)$r.squared,sd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    #             , summary(Model)$Std.error,summary(Model)$t valuesd(resid(Model))),line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    mtext(sprintf(paste0("General Additive model"))
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.9) 
    
  }
  if(Mod_type=='Quadratic') {
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY, model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = DataXY , weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Quadr.: y= ",f_coef1,"+ ",f_coef2,"x+ ",f_coef2,"x2",", R2=",f_R2,", s(Res)=",f_coef1,", RMSE=",f_coef1,",AIC= %.1f")
                  ,coef(Model)[1],coef(Model)[2],coef(Model)[3] ,summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
  }
  if(Mod_type=='Cubic') {
    if(is.null(s_y)|| any(s_y == 0)) {
      Model <- lm(y ~ poly(x, 3, raw =TRUE), data =DataXY, model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- lm(y ~ poly(x, 3, raw=TRUE) , data =DataXY, weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Cubic: y= ",f_coef1,"+",f_coef2,"x+",f_coef2,"x2+",f_coef2,"x3",",R2=",f_R2,", s(Res)=",f_coef1,",RMSE=", f_coef1,",AIC= %.1f"), 
                  coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4],
                  summary(Model)$r.squared,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model)),
          line=line_position,adj=1,padj=0,col=Couleur,cex=0.875) 
  }
  if(Mod_type=='ExpDecayInc') {
    if(is.null(s_y)|| any(s_y == 0)) {
      Model <- nlsLM(y~f_ExpDI(x,C,k), data=DataXY, start=list(C=max(y), k=0.05), model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nlsLM(y~f_ExpDI(x,C,k), data=DataXY, start=list(C=max(y), k=0.05), weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef1,"(1-exp(-",f_coef2,"x)), s(Res)=",f_coef1,",RMSE=",f_coef1,",AIC= %.1f"),
                  coef(Model)[1],coef(Model)[2],sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='ExpDecayInc_Int') {
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- nlsLM(y~f_ExpDI_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nlsLM(y~f_ExpDI_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Exp. decay inc.: ys = ",f_coef2,"(1-exp(-",f_coef2,"x))+",f_coef1,", s(Res)=",f_coef1,",RMSE=",
                         f_coef1,",AIC= %.1f"),coef(Model)[1],coef(Model)[2],coef(Model)[3], sd(resid(Model)), 
                  sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model)), line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='ExpDecayDec_Int') {
    if(is.null(s_y)|| any(s_y == 0)) {
      Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nls(y~f_ExpDD_Int(x, C, k,intercept), data=DataXY, start=list(C=max(y), k=0.05, intercept=min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Exp. decay dec: y =",f_coef2,"exp(-",f_coef2,"x))+",f_coef1,", s(Res)=",f_coef1,",RMSE=",
                         f_coef1,",AIC= %.1f"), coef(Model)[1],coef(Model)[2],coef(Model)[3], sd(resid(Model)), 
                  sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model)), line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='Michelis') {
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- nlsLM(y ~ MIN + f_Michelis(x, Vmax, km, intercept), data=DataXY, start=list(Vmax=max(y), km=mean(y)/4, MIN = min(y)), model = TRUE, x = TRUE, y = TRUE)
      #Model <- nlsLM(y~ MIN + SSmicmen(x, Vmax, km), data=DataXY, start=list(Min = min(y),getInitial(y~ SSmicmen(x, Vmax, km), data=DataXY)))
    } else {
      Model <- nlsLM(y ~ f_Michelis(x, Vmax, km, MIN), data=DataXY, start=list(Vmax=max(y), km=mean(y)/4, MIN = min(y)), weights = wi, model = TRUE, x = TRUE, y = TRUE)
      #Model <- nlsLM(y~ MIN + SSmicmen(x, Vmax, km), data=DataXY, weights = wi)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Michelis: y =",f_coef2,"/(",f_coef2,"+x)+",f_coef1,", s(Res)=",
                         f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), coef(Model)[1],coef(Model)[2],coef(Model)[3],
                  sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='Logarithmic') {
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- nls(y~f_log(x,a,b), data=DataXY, start=list(a=min(y), b=10), model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nls(y~f_log(x,a,b), data=DataXY, start=list(a=min(y), b=10), weights = wi, model = TRUE, x = TRUE, y = TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    DataXY <- DataXY[order(DataXY$x),] # To avoid that the line go back for lower x
    Estimated.y(DataXY$x, Model) 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Log. model: y = ",f_coef1," + ",f_coef2," log(x)), s(Res)=",
                         f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), coef(Model)[1],coef(Model)[2], sd(resid(Model)), 
                  sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model)),
          line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='Sigmoid') {
    #nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/4096, printEval = TRUE, warnOnly = TRUE)
    if(is.null(s_y) || any(s_y == 0)) {
      # Model <- nls(y~f_Sigmoid(x, MIN, MAX, X50, Hill), data=DataXY, start=list(MIN=min(y),MAX=max(y),X50=mean(x), Hill=3)
      #               , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE)
      Model <- nlsLM(y~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), Hill=3)
                     , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nlsLM(y~ f_Sigmoid(x, MIN, Asym, xmid, Hill), data=DataXY, start=list(MIN=min(y),Asym=max(y),xmid=mean(x), Hill=3), weights = wi
                     , control = list(maxiter = 500, tol=1e-2, minFactor = 1/1024, printEval = TRUE, warnOnly=FALSE), trace=TRUE, model = TRUE, x = TRUE, y = TRUE)
      # Model <- nls(y~ MIN + SSlogis(x, Asym, xmid, scal) , data=DataXY, start=list(MIN=min(y)), weights = wi
      #             , control = list(maxiter = 500, tol=1e-2, minFactor = 1/(1024*32), printEval = TRUE, warnOnly=FALSE), trace=TRUE)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Sigmoid: y=",f_coef1,"+",f_coef2,"/(1+(",f_coef2,"/x)^",f_coef2,"), s(Res)=",
                         f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), coef(Model)[1],coef(Model)[2] - coef(Model)[1],coef(Model)[3],coef(Model)[4],
                  sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model)),
          line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  if(Mod_type=='Unitec') {
    #Put O3, sensor reponses and standard deviation of sensor responses in a matrix: Not done for now
    a=-31.6
    b=5330.9
    c=-0.598
    DataXY <- data.frame(cbind(x*2.05, ((y-a)/b)^(1/c), s_y))
    colnames(DataXY) <- c("x","y")
    if(is.null(s_y) || any(s_y == 0)) {
      Model <- nls(y~f_Unitec(x, a, b,c), data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), model = TRUE, x = TRUE, y = TRUE)
    } else {
      Model <- nls(y~f_Unitec(x,a,b,c),data=DataXY, start=list(a=-31.6,b=5330.9,-0.598), weights = w, model = TRUE, x = TRUE, y = TRUEi)
    }
    print(summary(Model))
    
    # plotting calibrationn lines without plotting axis
    Estimated <- Estimated.y(DataXY$x, Model) 
    par(new=TRUE)#, mar=Margin)
    plot(Estimated$x,Estimated$y, col = Couleur, xlim = lim[,1], ylim = lim[,2], type= "l",xaxt ="n", yaxt = "n" , xlab= "", ylab = "") 
    
    # display equations and R^2
    mtext(sprintf(paste0(Sensor_name, "Unitec: y = ((1.91x(293.15/T)-",f_coef1,")/",f_coef2,")^(1/",f_coef21,"), s(Res)=",
                         f_coef1,",RMSE=",f_coef1,",AIC= %.1f"), coef(Model)[1],coef(Model)[2],coef(Model)[3],coef(Model)[4]
                  ,sd(resid(Model)), sqrt(sum(resid(Model)^2)/(length(resid(Model))-2)),AIC(Model))
          ,line=line_position,adj=1,padj=0,col=Couleur,cex=0.875)
  }
  summary(Model)
  # resuming the par values
  on.exit(par(op))
  
  return(Model)
}

##############################################################################
### Function Measurement Function x = f(y) once Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
##############################################################################
Meas_Function <- function (y, Mod_type, Model, covariates = NULL) {
  # This function correct the x variable according to a calibration model
  # x : data to be corrected with the calibration function (Model)
  # Mod_type : type of calibration function: Linear, Quadratic, Sigmoid
  # Model : the calibration function
  # Covariates: vectors of strings representing the covariates when model needs covariates
  #browser()
  if(Mod_type =='Linear' || Mod_type == 'Linear.Robust') {
    return((y-coef(Model)[1])/coef(Model)[2])
  }
  if(Mod_type=='Quadratic') {
    # le choix de la racine du polynome depend du signe du coefficient du monome de 2eme degre
    # et si l'on utilise la partie croissante ou decroissante de la parabole
    # Si le seterminat est n?gatif : NA
    
    x <- data.frame(y=y,determinant = coef(Model)[2]^2-4*coef(Model)[3]*(coef(Model)[1]-y), root = NA, stringsAsFactors = FALSE)
    if(coef(Model)[2] > 0){
      x[x$determinant>0 ,"root"] <- (-coef(Model)[2]+sqrt(x[x$determinant>0,"determinant"]))/(2*coef(Model)[3])
    } else {
      x[x$determinant>0 ,"root"] <- (-coef(Model)[2]-sqrt(x[x$determinant>0,"determinant"]))/(2*coef(Model)[3])
    }
    return(x$root)
  }
  if(Mod_type=='Cubic') {
    require(polynom)
    xroot  <- data.frame(y=y, root1=NA, root2=NA, root3=NA, stringsAsFactors = FALSE)
    
    xroot[,2] <- apply(xroot, MARGIN = 1, function(x){
      # Solving cubic equation using the eigen numbers of package polynom
      # for x^3-8 = 0 , it is the same as:
      # a <- c(0,0,8)
      # m <- matrix(c(0,0,-a[1],1,0,-a[2],0,1,-a[3]), byrow=T, nrow=3)
      # roots <- eigen(m, symm=F, only.values=T)$values
      # see https://stackoverflow.com/questions/2003465/fastest-numerical-solution-of-a-real-cubic-polynomial
      
      p <- polynom::polynomial(c(coef(Model)[1]-x[1],coef(Model)[2],coef(Model)[3],coef(Model)[4]))
      x[2:4] <- solve(p)
      #print(x)
      
      # Discarding complex number, and negative values
      for(j in 2:4) {
        if(Im(x[j])!=0 || Re(x[j]) < 0) x[j] <- NA else x[j] <- as.numeric(Re(x[j]))
      }
      #print(x)
      
      # Returning only one root, if more than one, take the smallest one non negative
      #browser()
      if(any(!is.na(x[2:4]))){
        if(length(which(!is.na(x[2:4])))==1) x[2] <- x[which(!is.na(x[2:4]))] else x[2] <- min(x[which(!is.na(x[2:4]))], na.rm = TRUE)  
      } 
      #print(Re(x[2]))
      return(Re(x[2]))
    })
    
    return(xroot[,2])
  }
  if(Mod_type=='Exponential') {
    return(-log(1-(y - coef(Model)[3])/coef(Model)[1])/coef(Model)[2])
  }
  if(Mod_type=='ExpDecayInc_Int') {
    if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1] + coef(Model)[3])) { # in case value out of bound and value is not NA
      print("Some Y value out of limits of the model", quote = FALSE)
    }
    x <- log(1-((y - coef(Model)[3])/coef(Model)[1]))/-coef(Model)[2]
    #  yy <- coef(Model)[1] * (1-exp(-coef(Model)[2] * xx)) + coef(Model)[3]
    return(x)
  }
  if(Mod_type=='Michelis') {
    # modele f_Michelis: return(Vmax*x/(km +x) + intercept)
    if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1])) { # in case value out of bound and value is not NA
      print("Some Y value out of limits of the model", quote = FALSE)
    }
    x <- (-coef(Model)[2]*(y - coef(Model)[3])) / (y - coef(Model)[3] - coef(Model)[1])
    #  yy <- coef(Model)[1] * (1-exp(-coef(Model)[2] * xx)) + coef(Model)[3]
    return(x)
  }
  if(Mod_type=='ExpDecayDec_Int') {
    # return(y = C *(exp(-k*x)) + intercept)
    if(any(!is.na(y) & y<coef(Model)[3] ) | any(!is.na(y) & y>coef(Model)[1] + coef(Model)[3])) { # in case value out of bound and value is not NA
      print("Some Y value out of limits of the model", quote = FALSE)
    }
    x <- log((y - coef(Model)[3])/coef(Model)[1])/-coef(Model)[2]
    return(x)
  }
  if(Mod_type=="Sigmoid") {
    if(any(!is.na(y) & y<coef(Model)[1]) | any(!is.na(y) & y > coef(Model)[2])) { # in case value out of bound and value is not NA
      print("Some Y value out of the limits of the model", quote = FALSE)
    }
    #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
    x <- coef(Model)[3]*((y-coef(Model)[1])/(coef(Model)[2]-y))^(1/coef(Model)[4])
    return(x)
  }
  if(Mod_type=="Logarithmic") {
    if(y<coef(Model)[1] )  {
      #  return(y = MIN + (MAX-MIN) / (1 + (X50/x)^Hill))
      x <- exp((y-coef(Model)[1])/coef(Model)[2])
      return(x)
    } else {
      print("y values out of limits", quote = FALSE)
    }
  }
}

##############################################################################
### MeasLinear: Function Measurement Function x = f(y) once the Linear Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
##############################################################################
MeasLinear <- function(x, ModLinear) {
  (x-coef(ModLinear)[1])/coef(ModLinear)[2]
}

##############################################################################
### MeasParab: Function Measurement Function x = f(y) once the Quadratic Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
##############################################################################
MeasParab <- function(x, ModParab) {
  if(coef(ModParab)[2] > 0){
    return((-coef(ModParab)[2]+sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
  } else {
    return((-coef(ModParab)[2]-sqrt(coef(ModParab)[2]^2-4*coef(ModParab)[3]*(coef(ModParab)[1]-x)))/(2*coef(ModParab)[3]))
  }
}

##############################################################################
### MeasSigmoid: Function Measurement Function x = f(y) once the Sigmoidal Calibration function (y = f(x) of sensor is established e.g with Cal_Line (Vs 141114)
##############################################################################
MeasSigmoid <- function(x, ModSigmoid) {
  (log((coef(ModSigmoid)[1]-coef(ModSigmoid)[4])/(x-coef(ModSigmoid)[4])-1)/(-coef(ModSigmoid)[2])+coef(ModSigmoid)[3])
}

##############################################################################
### Function Significant numbers (Vs 141114)
##############################################################################
sigdigss<-function(n) {
  i <- 0
  # Check for decimal point is present
  if(length(grep("\\.", numstr[n])) > 0) { # real number
    # Separate integer and fractional parts
    intfrac <- unlist(strsplit(numstr[n], "\\."))
    digstring <- paste(intfrac[1], intfrac[2], sep = "")
    numfigs <- nchar(digstring)
    while(i < numfigs) {
      # Find index of 1st non-zero digit from LEFT
      if(substr(digstring,i+1,i+1) == "0") {
        i <- i + 1
        next
      } else {
        sigfigs <- numfigs - i
        break
      }
    }   
  } else {  # must be an integer      
    digstring <- numstr[n]
    numfigs <- nchar(digstring)
    while(i < numfigs) {
      # Find index of 1st non-zero digit from RIGHT
      if(substr(digstring, numfigs-i, numfigs-i) == "0") {
        i <- i + 1
        next
      } else {
        sigfigs <- numfigs - i
        break
      }
    }   
  }   
  return(sigfigs)
}

##############################################################################
### Function to fit the best polynomial fitting (Vs > 151016)
##############################################################################
polyfit <- function (XY, i) {
  # The function returns the degree of the polynomila with first minimum of AIC for a set of n-1 degree polynomials with maximum degree i:
  # may not have a single minimum. check this with something like
  # XY: dataframe with x and Y values
  
  # removing NA
  XY <- na.omit(XY)
  
  #limiting i to the number of row in XY -1
  i  <- min(i, nrow(XY)-1)
  
  polyfit <- function(i) x <- AIC(lm(XY[,2] ~ poly(XY[,1],i))) # Should be outside the function
  degre <- as.integer(optimize(polyfit,interval = c(1,i))$minimum)
  
  print(sprintf("Best degree of polynomial y = f8x): %.0f: ", degre), quote=FALSE)
  for (j in 1:i) {
    print(sprintf("Polynomial of degree %.0f: AIC %.3f", j, polyfit(j)), quote =  FALSE)
  } 
  
  return(degre)
  
}

##############################################################################
### Functions for fiting distributions
##############################################################################

##########################################################
# Function probability distribution funtion for Log-Normal distribution 1 parameter
##########################################################
f_lnormal_1par <- function(epsilon, lambda, q, m) {
  # epsilon: vector data for wwhich the log-normal distribution is fitted
  # lambda: vector shape parameter
  # q: lower limiting value
  # m: median
  return(
    1/(sqrt(2*pi)*lambda*abs(epsilon-q)) * exp(-(log((epsilon-q)/(m-q)))^2/(2*lambda^2))
  )
}
##########################################################
# Function probability distribution funtion for Log-Normal distribution 2 parameters
##########################################################
f_lnorm_2Par <- function(x, mu, sigma){
  # x: vector data for wwhich the log-normal distribution is calculated
  # mu: mean of the log of x 
  # x: standard devaition of the log of x)
  # m: median
  return(1/(sqrt(2* pi)*sigma*x) * exp(-(log(x)-mu)^2/(2*sigma^2)))
}

##########################################################
# Function returning mode of a distribution
##########################################################
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##########################################################
# Fitting distribution to data
##########################################################
Fitting_distrib <- function(Air_Pol, ug, coeff_ppb, Dist_xlab, Dist_range, BandWidth, band, distribution, to_fit, lamb, q) {
  # fitting distribution manual Non Linear Regression based on the probability distribution funtion with only one parameter: the shape parameter
  # Air_Pol: variable for which the densit function is calculated
  # ug: if TRUE transform from ug/m3 to ppb or ppm if FALSE does not transform 
  # coeff_ppb: used if ug = TRUE: conversion factor to divide  ug/m3 to get ppb (ex 1.91 for NO2)
  # Dist_xlab: Label of X axis of the distribution plot
  # Dist_range: min and max value for the distribution plot - NO WE USE QUANTILE of 99 %
  # BandWidth: if TRUE bandwidth is automatically estimated by the R's "density" function. if FALSE the coeff band is used (see below)
  # band : USed if bandwith is FALSE, If 0 then bw is set to "nrd0" (see help of density function). If anoother values is passed, the bandwidth is set to this value
  # distribution: type of distribution to be fitted: lognormal ...
  # to_fit: if TRUE fitted with nls, if FALSE: not fitted - value lamb is used for the shape factor
  # lamb: initial lambda value for the lognormal distribution
  # q the limit of possible data of log normal distribution
  
  # removing negative values of data
  # Air_Pol <- subset(Air_Pol, Air_Pol >0)
  # transforming in ppb
  if (ug) Air_Pol <- Air_Pol/coeff_ppb
  
  #-----------------------------------
  # calculating the reference density
  #-----------------------------------
  if (BandWidth == TRUE) {
    # calculating the band width of reference distribution
    h <- density(Air_Pol, kernel="gaussian")$bw
    # calculating weight that are higher at 0
    print(sprintf("Best bandwidth = %.3f", h), quote = FALSE)
    d_Air_Pol<- density(Air_Pol, bw = h, adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data 
  }
  else {
    if (band == 0) {
      d_Air_Pol<- density(Air_Pol, bw = "nrd0", adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data 
      h <- d_Air_Pol$bw
    }
    else {
      d_Air_Pol<- density(Air_Pol, bw = band, adjust = 1 , kernel = "gaussian", na.rm = TRUE) # returns the density data 
      h <- d_Air_Pol$bw
    }
  }
  print(sprintf("Bandwith : %.6f",h))
  
  #-----------------------------------
  # Subtracting the minimum d_Air_Pol$x if negative - Defining "Air_Pol_final"
  #-----------------------------------
  if (min(d_Air_Pol$x)<-10000) {
    Air_Pol_final <- Air_Pol - min(d_Air_Pol$x) + 0.0001
    print(sprintf("minmum x_density: %.3f was subtracted",min(d_Air_Pol$x)))
    d_Air_Pol<- density(Air_Pol_final, bw = h, adjust = 1, kernel = "gaussian", na.rm = TRUE) # returns the density data 
    print(sprintf("minmum x_density: %.3f",min(d_Air_Pol$x)))
  }
  else {
    print(sprintf("minmum x_density: %.1f was not added",min(d_Air_Pol$x)))
    Air_Pol_final <- Air_Pol # do not add anything becasue the minimum value is not negative
  }
  
  #-----------------------------------
  ## density plot of d_Air_Pol - one parameter - reference density plot
  #-----------------------------------
  par(mar=c(5, 0.5, 0, 0.5))
  plot(d_Air_Pol, xlim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)), xlab = Dist_xlab, ylab = "", main ="", cex.lab=3, cex.axis=2.5)
  polygon(c(min(d_Air_Pol$x),d_Air_Pol$x,quantile(Air_Pol, probs = 0.98)), c(0,d_Air_Pol$y,0), col="red", border="blue")
  
  #-----------------------------------
  # calculating the Mode of the data variable
  ## with the Fit of a log-normal distribution With MASS function fitdistr
  #-----------------------------------
  f_MASS <- fitdistr(Air_Pol_final, "log-normal")
  coef(f_MASS)
  M <- lnormMode(meanlog = coef(f_MASS)['meanlog'], sdlog = coef(f_MASS)['sdlog']) # need package modeest  
  
  #transforamtion in log
  Log_Air_Pol_final <- log(Air_Pol_final)
  #-----------------------------------
  ## Histogram and density plots for the 2 parameters Lognormal distribution
  #-----------------------------------
  hist(Log_Air_Pol_final)
  mu <- coef(f_MASS)['meanlog']
  sigma <- coef(f_MASS)['sdlog']
  x <- seq( min(Air_Pol_final, na.rm = T), quantile(Air_Pol_final,probs = 0.98), length.out=250)
  y <- f_lnorm_2Par(x,mu,sigma)
  par(mar=c(5, 0,0, 0))
  plot(x,y, xlim = c(min(0,x,na.rm = T),max(x,na.rm = T)), xlab = Dist_xlab, ylab = "", main ="", type = "l", cex.lab=3, cex.axis=2.5)
  polygon(c(min(x),x,max(x)), c(0,y,0), col="red", border="blue")
  abline(v = M, lwd =2, col = "Black")
  print(sprintf("Mu (mean of log): %.3f", mu), quote = FALSE)
  print(sprintf("Sigma (sd of log): %.3f",sigma), quote = FALSE)
  print(sprintf("Mode: %.3f",exp(mu-sigma^2)), quote = FALSE)
  print(sprintf("Median: %.3f",median(x)), quote = FALSE)
  print(sprintf("u (adding the mode): %.3f",sqrt(exp(mu+1/2*sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))), quote = FALSE)
  
  # median 
  m <- median(Air_Pol_final) 
  #-----------------------------------
  # Fitting Distribution of NASA
  #-----------------------------------
  # if (distribution == "lognormal") {
  #     
  #     # Estimating the Mode as the maximum of the density function - Document NASA
  #     M <- mlv(Air_Pol_final, method = "parzen", bw = h)
  #     # Preparing data for fitting the distribution without NA and negative values, adding weights according to distance from 0 to promote data at 0
  #     DataXY <- na.omit(data.frame(cbind(d_Air_Pol$y, d_Air_Pol$x, d_Air_Pol$y^-1)))
  #     colnames(DataXY) <- c('referenceY','x_to_fit', "w")
  #     DataXY$W <- d_Air_Pol$y^-1 / sum(d_Air_Pol$y^-1)                  
  #     # Fitting the distribution with non linear regression for NO2
  #     # initial value of lambda = 1.0
  #     if (to_fit) {
  #         fitted_Distribution <- nls(referenceY ~ f_lnormal_1par(x_to_fit, lambda, q, m)
  #                                    , data=DataXY, start=list(lambda = coef(f_MASS)['sdlog'])) # start=list(lambda = lamb)
  #         lambda <- coef(fitted_Distribution)
  #     }
  #     else {
  #         lambda <- lamb
  #     }
  #     DataXY$Fitted <- f_lnormal_1par(DataXY$x_to_fit, lambda, q, m)
  #     # if NA are created:
  #     DataXY <- na.omit(DataXY)
  #     
  #     #-----------------------------------
  #     # calculating the Mode of the data variable
  #     ## with the Fit of a log-normal distribution With MASS function fitdistr
  #     #-----------------------------------
  #     f_MASS <- fitdistr(Air_Pol_final, "log-normal")
  #     coef(f_MASS)
  #     M <- lnormMode(meanlog = coef(f_MASS)['meanlog'], sdlog = coef(f_MASS)['sdlog']) # need package modeest  
  #     # Plotting f_MASS
  #     par(mar=c(5, 0.5, 0, 0.5))
  #     curve(dlnorm(x, meanlog = coef(f_MASS)["meanlog"], sdlog = coef(f_MASS)["sdlog"]), from = 0, to = quantile(Air_Pol_final, probs = 0.98)
  #           , xlab = Dist_xlab, ylab =""
  #           , main ="",cex.lab=3, cex.axis=2.5)
  #     abline(v = M, lwd =2, col = "Black")
  #     par(new=T)
  #     
  #     # Printing characteristics of the lognormal distribution
  #     print(sprintf('Mode: M = %.3f', M), quote = FALSE)
  #     print(sprintf('Median: m = %.3f', m), quote = FALSE)
  #     print(sprintf('Lower limit of possible values: q = %.3f', q), quote = FALSE)
  #     print(sprintf('Shape factor: Lambda = %.4f',  lambda), quote = FALSE)
  #     print(sprintf('Standard uncertainty: u = %.3f',  sqrt(M^2 + (abs(m+q) * exp(lambda^2) * sqrt(exp(lambda^2-1)))^2), quote = FALSE))
  #     # plotting the refrence distribution and fittecd one 
  #     print(sprintf('Max reference %.1f, max fitted %.1f', max(DataXY$referenceY), max(DataXY$referenceY,DataXY$Fitted)), quote = FALSE)
  #     ## density plot of d_Air_Pol - one parameter - reference density plot
  #     par(mar=c(5, 0.5, 0, 0.5))
  #     Ylim <- c(0, quantile(Air_Pol_final, probs = 0.98)) 
  #     plot(DataXY$x_to_fit, DataXY$referenceY
  #          , type ="l"
  #          , xlim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)) 
  #          , ylim = Ylim
  #          , xlab = Dist_xlab, ylab =""
  #          , main ="",cex.lab=3, cex.axis=2.5) 
  #     polygon(c(min(DataXY$x_to_fit),DataXY$x_to_fit,max(DataXY$x_to_fit)), c(0,DataXY$referenceY,0), col="red", border="blue")
  #     lines(DataXY$x_to_fit, DataXY$Fitted , 
  #           xlim = Dist_range, 
  #           ylim = c(min(0,d_Air_Pol$x),quantile(Air_Pol, probs = 0.98)), 
  #           xlab = Dist_xlab, 
  #           ylab ="", 
  #           lwd =2, 
  #           col = "Blue"
  #     )
  #     abline(v = M, lwd =2, col = "Black")
  # }
  #return(lambda)
  return(c(m, M,sqrt(exp(mu+1/2*sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))))  
} 

##########################################################
# Fitting the distribution to data
##########################################################
Fit_Hist <- function(Air_Pol, ug, coeff_ppb, Dist_range, bins, distribution, to_fit, Dist_xlab) {
  # fitting distribution manual Non Linear Regression based on the probability distribution funtion with only one parameter: the shape parameter
  # data: variable for whcih the densit function is calculated
  # ug: if TRUE transform from ug/m3 to ppb or ppm if FALSE does not transform 
  # coeff_ppb: used if ug = TRUE: conversion factor to divide  ug/m3 to get ppb (ex 1.91 for NO2)
  # Dist_range: min and max value for the distribution plot and histogram
  # bins: number of breaks in the histogram
  # distribution: type of distribution to be fitted: lognormal ...
  # to_fit: if TRUE fitted with nls, if FALSE: not fitted
  # Dist_xlab: Label of X axis of the distribution plot
  
  # removing negative values of data - No we do not do it
  # Air_Pol <- subset(Air_Pol, Air_Pol >0)
  # transforming in ppb
  if (ug) Air_Pol <- Air_Pol/coeff_ppb
  
  # calculating the reference histrogram
  par(mar=c(5, 2,0, 0))
  histogram = hist(Air_Pol, breaks = bins, xlim =c(mini,maxi),xlab = Dist_xlab, ylab = "", main ="", cex.lab=3, cex.axis=2.5)
  # histogram <- subset(histogram, histogram$mids>0) # removing the negative values
  
  
  ## density plot for the 2 parameters lognormal distribution
  Log_Air_Pol <- log(Air_Pol)
  mu <- mean(Log_Air_Pol)
  sigma <- sd(Log_Air_Pol)
  x <- seq( min(Air_Pol), max(Air_Pol), length.out=250)
  y <- f_lnorm_2Par(x,mu,sigma)
  par(mar=c(5, 2, 0, 0))
  plot(x, y, xlim = c(min(0,x),max(x)), xlab = Dist_xlab, ylab = "", main ="", type = "l", cex.lab=3, cex.axis=2.5)
  polygon(c(min(x),x,max(x)), c(0,y,0), col="red", border="blue")
  
  print(sprintf("Mu (mean of log): %.1f", mu), quote = FALSE)
  print(sprintf("Sigma (sd of log): %.1f",sigma), quote = FALSE)
  print(sprintf("Mode: %.1f",exp(mu-sigma^2)), quote = FALSE)
  print(sprintf("Median: %.1f",median(Air_Pol)), quote = FALSE)
  print(sprintf("u (adding the mode): %.1f",sqrt(exp(mu-sigma^2)^2 + exp(2*(mu+sigma^2)) - exp(2*mu+sigma^2))), quote = FALSE)
  
  # Fitting Distribution
  if (distribution == "lognormal") {
    # Preparing data for fitting the distribution without NA and negative values, adding weights according to distance from 0 to promote data at 0
    DataXY <- na.omit(data.frame(cbind(histogram$density, histogram$mids)))
    colnames(DataXY) <- c('referenceY','x_to_fit')
    # Fitting the distribution with non linear regression with initial values of mu and sigma 
    if (to_fit) {
      fitted_Distribution <- nls(referenceY ~ f_lnormal_2par(x_to_fit, m, s)
                                 , data=DataXY, start=list(m = mu, s = sigma))
      mu <- coef(fitted_Distribution)[1]
      sigma <- coef(fitted_Distribution)[2]
    }
    else {
      mu <- mean(Log_Air_Pol)
      sigma <- sd(Log_Air_Pol)
    }
    DataXY$Fitted <- f_lnormal_2par(DataXY$x_to_fit, mu, sigma)
    # if NA are created:
    DataXY <- na.omit(DataXY)
    # calculating the Mode of the data variable
    M <-  exp(mu - sigma^2)
    # Printing characteristics of the lognormal distribution
    
    # plotting the refrence distribution and fittecd one 
    print(sprintf('Max reference %.1f, max fitted %.1f', max(DataXY$referenceY), max(DataXY$referenceY,DataXY$Fitted)), quote = FALSE)
    plot(DataXY$x_to_fit, DataXY$referenceY
         , type ="l"
         , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted))
         , xlab = Dist_xlab, ylab =""
         , main ="",cex.lab=3, cex.axis=2.5) 
    lines(DataXY$x_to_fit, DataXY$Fitted , xlim = Dist_range, ylim = c(0, max(DataXY$referenceY,DataXY$Fitted)), xlab = Dist_xlab, ylab =""
    )
    abline(v = M, col = 2)
  }
  return(fitted_Distribution)  
} 

##############################################################################
### Change names of files
##############################################################################
umxRenameFile <- function(baseFolder = "Finder", Missing = FALSE, findStr = NA, missingStr = NA, replaceStr = NA
                          , listPattern = NA, test = T, overwrite = F, Split = "_", Position = 2) {
  # see http://stackoverflow.com/questions/10758965/how-do-i-rename-files-using-r#10759083
  # If Missing = TRUE adding replaceStr at the positiona and split of the filename, If Missing = FALSE replacing findStr with replaceStr in the filenames
  # Split = "_", Position = 2 to lacate the position where to add test if Missing = TRUE
  # uppercase = u$1
  if(baseFolder == "Finder"){
    baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
    message("Using front-most Finder window:", baseFolder)
  } else if(baseFolder == "") {
    baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep = "") ## choose a directory
    message("Using selected folder:", baseFolder)
  }
  if(is.na(listPattern)){
    listPattern = findStr
  }
  a = list.files(baseFolder, pattern = listPattern)
  changed = 0
  if(!Missing){ # it is a replacement not a missing String
    message("found ", length(a), " possible files")
    for (fn in a) {
      findB = grepl(pattern = findStr, fn) # returns 1 if found
      cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
      if(findB){
        fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
        if(test){
          message("would change ", fn, " to ", fnew)  
        } else {
          if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
            message("renaming ", fn, " to ", fnew, "failed as already exists. To overwrite set T")
          } else {
            file.rename(file.path(baseFolder, fn), file.path(baseFolder, fnew))
            changed = changed + 1;
            message("renamed ", fn, " to ", fnew, ".")
          }
        }
      }else{
        if(test){
          message(paste("bad file",fn))
        }
      }
    }    
  }  else { # it is a missing String added at position 2 splitted with Split of the file name
    a = a[grep(pattern = replaceStr, x = a, invert = TRUE)]
    message("found ", length(a), " possible files")
    for(fn in a) {
      #cat(strsplit(fn, split = "_")[[1]][2], sep = "\n")
      fnew = gsub(pattern = strsplit(fn, split = "_")[[1]][1], replace = paste0(strsplit(fn, split = "_")[[1]][1],"_",replaceStr), fn) # replace all instances
      if(test){
        message("would change ", fn, " to ", fnew)  
      } else {
        if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
          message("renaming ", fn, " to ", fnew, "failed as already exists. To overwrite set T")
        } else {
          file.rename(file.path(baseFolder, fn), file.path(baseFolder, fnew))
          changed = changed + 1;
          message("renamed ", fn, " to ", fnew, ".")
        }
      }
    }    
  }
  message("changed ", changed)
}

##############################################################################
### Function checking if a value is within a range
##############################################################################
is.between <- function(x, mini, maxi) {
  ## This function check if the value x is found within a and b. 
  ## copyright: https://stat.ethz.ch/pipermail/r-help/2008-August/170749.html
  ## x: value to be checked
  ## mini: min of the range, usually min() can be used to automatise the process
  ## maxi: mas of the range, usually max() can be used to automatise the process
  
  (x - mini)  *  (maxi - x) > 0
}

##############################################################################
### Function loading an RData file and returning it into a variable
##############################################################################
loadRData <- function(fileName){
  ## This function loads an RData file and returns it into a variable
  ## This function is particularly usefull if you don't know the name of the dataframe included in the .RData
  ## or if this name change from file to file.
  ## copyright: http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  ## fileName: file path of the RData file
  
  load(fileName)
  get(ls()[ls() != "fileName"])
}

##############################################################################
#### Function replace NaN with NA in a database
##############################################################################
# source: https://rdrr.io/github/mdtrinh/vietnamdata/man/is.nan.data.frame.html
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
