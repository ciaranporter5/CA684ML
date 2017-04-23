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

2. WeatherImputation.R

This script loads the semi-cleaned weather data from Excel to impute missing values. These take 2 forms, records with no values and records with some missing values. First, records with all values missing have important fields, i.e. Temp and Dew Point imputed by Time Series method. Once complete all records should only be missing values across some fields. Random Forest imputation is used to complete these.

3. TaxiPreProcessing.R

This script loads the final data set after all required cleansing, imputation and transformations have been made. Indicator variables are added to assist with categorical data, scaling is applied to the density variables, and the geohash features are given numeric ranked values so they can be placed in an ordered list as inputs.

<b>Main</b>

1. TaxiLM.R

Runs linear regression models on the Taxi dataset, and compares the impact of using different features on the Root Mean Squared Error (RMSE) and the Mean Absolute Error (MAE).

2. Taxi.NaiveBayes.R

Runs various tests required to determine whether feature variables are normally distributed, and applies conversion to Gaussian where necessary, as this is a pre-requisite for the Naive Bayes model. 

Uses different combinations of feature variables to investigate which model gives the highest level of accuracy on the test dataset.

3. TaxiPickupNN.R

Execution of neural networks on training and test data. Highlights features and parameters used for prediction, and tests the accuracy of the model using the Root Mean Squared Error (RMSE) and the Mean Absolute Error (MAE).

4. TaxiPickupSVM.R

Execution of support vector machines on training and test data for a one month sample. Highlights features and parameters used for prediction, and tests the accuracy of the model using the Root Mean Squared Error (RMSE) and the Mean Absolute Error (MAE).

5. Taxi.DecisionTree.R

Uses the <i>rpart</i> library in R to derive decision trees on the Taxi dataset. Different combinations of features variables are tested, and different levels of pruning are applied to the trees to determine an optimum model. 

6. RandomForest.R

Runs a random forest based of 3 sets of 50 distinct decision trees which are combined together and estimates the significance of each feature used. As per usual, model error metrics such as Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE) are performed on the outputted model.
