<b>CA684 Machine Learning Repository</b>

This page acts as a Repository for Machine Learning Taxi Assignment

All source code used as part of data cleaning and testing of the various models is included here.

<b>SQL Scripts</b>
1. Taxi_Query.sql

Preprocessing using SQL Server was performed to combine the six months worth of taxi journeys into a single dataset. 
Distinct pickup locations are extracted out to assist with later geohashing in R. Distinct time intervals (of half-hour duration) were also derived.

This script also combines the existing raw data with its corresponding geohashed location data and summarises the extract on pickup date, time and location. This allows for the overall pickup density to be calculated, which is what acts as the main prediction variable. In addition to this, the lagged pickup densities (trips in last half hour, hour, day and week) as well as local weather data are included.

<b>R Scripts</b>

<b>Preparation</b>

1. TaxiGeoHashing.R

Derives the geohashes for distinct pickup latitudes and longitudes using the <i>geohash</i> library in R. This is required so that pickups in similar locations can be grouped together.

2. Imputation of weather data

3. TaxiPreProcessing.R

This script loads the final data set after all required cleansing, imputation and transformations have been made. Indicator variables are added to assist with categorical data, scaling is applied to the density variables, and the geohash features are given numeric ranked values so they can be placed in an ordered list as inputs.

Main
1. Linear Regression

-- some detail in here

2. Naive Bayes

-- some detail in here

3. TaxiPickupNN.R

Execution of neural networks on training and test data. Highlights features and parameters used for prediction, and tests the accuracy of the model using the Root Mean Squared Error (RMSE) and the Mean Absolute Error (MAE).

4. TaxiPickupSVM.R

Execution of support vector machines on training and test data for a one month sample. Highlights features and parameters used for prediction, and tests the accuracy of the model using the Root Mean Squared Error (RMSE) and the Mean Absolute Error (MAE).

5. Decision Tree

-- some detail in here

6. Random Forest

-- some detail in here
