CA684 Machine Learning Repository
This page acts as a Repository for Machine Learning Taxi Assignment

All source code used as part of data cleaning and testing of the various models is included here.

SQL Scripts
1. Taxi_Query.sql
Preprocessing using SQL Server was performed to combine the six months worth of taxi journeys into a single dataset. 
Distinct pickup locations are extracted out to assist with later geohashing in R. Distinct time intervals (of half-hour duration) were also derived.

This script also combines the existing raw data with its corresponding geohashed location data and summarises the extract on pickup date, time and location. This allows for the overall pickup density to be calculated, which is what acts as the main prediction variable. In addition to this, the lagged pickup densities (trips in last half hour, hour, day and week) as well as local weather data are included.

R Scripts

Prep
1. Geohashing of location

2. Imputation of weather data

Main
1. Neural Network
-- some detail in here

2. Support Vector Machine
-- some detail in here

3. Naive Bayes
-- some detail in here

etc
