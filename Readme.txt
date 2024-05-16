Readme SENSEURCITY CALC

These scripts correspond to the methodology described in Wesseling et al., 2024. INERIS approach. 

02_data_cleaning.R => this script allows to clean the sensor dataset: 
1)eliminate negative values
2)eliminate values > threshold value based on max reference station value
3)identify frozen values for more than 3 hours
4)eliminate sensors with positive constante bias
5)clean data are saved in OUTPUTS dir

03_data_classification.R => this script allows to make clusters of sensors to perform the outlier detection
1)sensor type is first defined based on the Corine Land Cover data (land use data)
2)sensor type is corrected based on population data
3)sensor type is corrected based on road network data
4)clustering is performed based on distance between sensors
5)groups of sensor data are created based on the type, the clusters, the seasons (possible to add time of the day)
6)sensor data with groups are saved in OUTPUTS dir

04_outliers_detection_v2.R => this script allows the outliers detection for each group of sensor data (add reference)
1)a square root transformation of the sensor data is applied
2)mean and standard deviation of the log-transformed distribution of the sensor data are calculated removing the ith observation
3)an optimization of the log likelihood function is performed to find the mean and standard deviation of the underlying normal distribution
4)a back transformation of the sensor data is done
5)a flag is assigned to outliers in the sensor dataframe
6)sensor data with outliers flags are saved in the OUTPUTS dir

05_calibration_RIVM.R => this script allows the calibration of the sensor data based on RIVM methodology
1)a loop over hours is done
2)sensors that are in the vicinity of reference stations are identified, creation of group of sensors for each reference station
3)a fator of correction is calculated at each station based on the selected sensors in 2)
4)an interpolation of the corection factors is performed
5)the interpolated factors are applied to the sensor data
6)the factors and the corrected sensor data are saved in the OUTPUTS dir

06_calibration_performance.R => this script allows the evaluation of the calibration step
1)Read data and plot the performance metrics

07_uncertainty_calc.R => this script allow to calculate sensor data uncertainties
This is adapted from the European equivalence demonstration guide for the use of sensor systems in ambient air environments:
1)a random noise associated to the sensors can be calculated (sensor-specific, to be adapted?)
2)a between sensor uncetainty is calculated (i.e. reproducibility between different replicas of the sensor)
3)a field uncetainty is calculated (incorporating the effects of parameters influencing the variability of the sensor system's response)
 
