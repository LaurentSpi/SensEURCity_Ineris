###############################  FAIRMODE CT6: Example code for calibrating PM2.5 sensors #######################
##
##
## FAIRMODE CT6: Example code for calibrating PM2.5 sensors by RIVM is licensed under CC BY-NC-SA 4.0 
##                                            [Attribution-NonCommercial-ShareAlike 4.0 International]
##
## Version 0.90, January, 21, 2021
##
##
###############################  FAIRMODE CT6: Example code for calibrating PM2.5 sensors #######################
Power_CF = 2
Dmin = 1
Offset_CF = 2.0
Par1_CF = 1.0
m_to_km = 0.001
Interpolate_REFs <- function(XX,YY){
  xCF_RMS = 150.0
  dx = m_to_km*(XX - calibrationFactorsForInterpolation$X)
  dy = m_to_km*(YY - calibrationFactorsForInterpolation$Y)
  d = sqrt(dx*dx + dy*dy)
  d[(d<Dmin)] = Dmin
  w = (calibrationFactorsForInterpolation$CalibrationFactor > 0.0)*1.0 / ((d+Offset_CF) / Par1_CF) +
      (calibrationFactorsForInterpolation$CalibrationFactor <= 0.0)*0.0
  w = w**(Power_CF) 
  sum1 = sum(w*calibrationFactorsForInterpolation$CalibrationFactor,na.rm = T)
  sum2 = sum(w,na.rm = T)
  if (sum2 > 0.0) {
    cf = sum1 / sum2
  } else {
    cf = 0.0
  }
  return (cf) 
}