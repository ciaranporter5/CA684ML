install.packages("psych")
install.packages("car")
install.packages("Hmisc")
install.packages("geohash")
install.packages("caTools")
install.packages("biglm")
install.packages("RcppArmadillo")
library("psych")
library("car")
library("Hmisc")
library("geohash")
library("caTools")
library("biglm")
library("RcppArmadillo")

#Set the working directory
setwd("C:\\Users\\lvanrheena007\\Documents\\DCU\\CA684 ML")

#read in taxi data
TaxiPickupSummary <- read.csv("Summarized_inc_weather_Final.CSV")

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]

#include in latitude and longitude information relevative to the geohashes
TaxiPickupSummary$ReversedLat <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lat
TaxiPickupSummary$ReversedLong <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lng

#Extract the unique lats and longs
DistinctLat <- unique(TaxiPickupSummary$ReversedLat) 
DistinctLong <- unique(TaxiPickupSummary$ReversedLong)
DistinctTime <- unique(TaxiPickupSummary$TimeInterval)

#Rank by lat descending and long ascending
RankedLat <- data.frame(DistinctLat, rank(-DistinctLat))
colnames(RankedLat)[1] <- "ReversedLat"

RankedLong <- data.frame(DistinctLong, rank(DistinctLong))
colnames(RankedLong)[1] <- "ReversedLong"

RankedTime <- data.frame(DistinctTime, rank(DistinctTime))
colnames(RankedTime)[1] <- "TimeInterval"

#Join back into TaxiPickupSummary table
TaxiPickupSummary <- merge(TaxiPickupSummary, RankedLat, by = "ReversedLat")
TaxiPickupSummary <- merge(TaxiPickupSummary, RankedLong, by = "ReversedLong")
TaxiPickupSummary <- merge(TaxiPickupSummary, RankedTime, by = "TimeInterval")
colnames(TaxiPickupSummary)[30] <- "RankLat"
colnames(TaxiPickupSummary)[31] <- "RankLong"
colnames(TaxiPickupSummary)[32] <- "RankTime"


# assign value to categorical variables based on month bin
for (currentMonth in unique(TaxiPickupSummary$Month)){
  if (currentMonth != "June"){
    TaxiPickupSummary[currentMonth] <- 
      ifelse(TaxiPickupSummary$Month==currentMonth,1,0)
  }
}

# set up binary variables for days for the week.
# Note that there are 6 variables. Sunday is assumed to be where all 6 equal 0
for (day in unique(TaxiPickupSummary$Week_Day)){
  if (day != "Sunday"){
    TaxiPickupSummary[day] <- ifelse(TaxiPickupSummary$Week_Day==day,1,0)
  }
}

# assign value to categorical variables based on time period bin
for (timePeriod in unique(TaxiPickupSummary$RankTime)){
  if (timePeriod != 1){
    TaxiPickupSummary[paste0("T",timePeriod)] <- 
      ifelse(TaxiPickupSummary$RankTime==timePeriod,1,0)
  }
}

# assign value to categorical variables based on conditions bin
for (currentConditions in gsub(" ", "_", unique(TaxiPickupSummary$Conditions))){
  if (currentConditions != "Clear"){
    TaxiPickupSummary[currentConditions] <- 
      ifelse(TaxiPickupSummary$Conditions==currentConditions,1,0)
  }
}

# assign value to categorical variables based on latitude bin
for (Lat in unique(TaxiPickupSummary$RankLat)){
  if (Lat != 36){
    TaxiPickupSummary[paste0("Lat", Lat)] <- 
      ifelse(TaxiPickupSummary$RankLat==Lat,1,0)
  }
}

# assign value to categorical variables based on longitude bin
for (Long in unique(TaxiPickupSummary$RankLong)){
  if (Long != 24){
    TaxiPickupSummary[paste0("Long", Long)] <- 
      ifelse(TaxiPickupSummary$RankLong==Long,1,0)
  }
}

# Select a subset of numeric variables for regression modelling
TaxiReg <- subset(TaxiPickupSummary, select = -c(ReversedLong, ReversedLat, pickupDate, Week_Day, Month, DAY, TimeInterval, pickup_Geohash, total_passenger_count, total_fare, Class, Wind_Dir, Events, Conditions, RankLat, RankLong, RankTime, Temp, Humidity, Wind_Speed, Light_Rain, Partly_Cloudy, Heavy_Rain, Heavy_Snow, Mostly_Cloudy, Light_Snow, Haze, Light_Freezing_Fog, Light_Freezing_Rain, Lat35, Lat34, Lat33, Lat5, Lat4, Lat3, Lat2, Lat1, Lat10, Lat28, Long21, Long22, Long23, Long18, Long3, Long8, Long13, Scattered_Clouds, Lat19, T2, T17, T18, T19, T20, T21, T22, T23, T24, T25, T26, T27, T28, T29, T30, T31, T32, T33, T34, T35, T36, T42, T44, T45, T46, T47, T48))

# Check assumptions of Linear Regression on main Variables
pairs.panels(TaxiReg[c(2,3,6,7,8,9,10,11,12)] , col="red")

# Set seed so that same training/test set is used on each run
set.seed(1)

# Split into training and test data based off of split Boolean Vector
TaxiReg$TaxiSplit <- sample.split(
  TaxiReg$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain <- subset(TaxiReg , TaxiSplit == TRUE)
TaxiTest <- subset(TaxiReg , TaxiSplit == FALSE)

# Create nam vector of variable names to be used by Linear Regression
nam <- formula(paste(colnames(TaxiTrain)[1], "~",
	  paste(colnames(TaxiTrain)[c(2:3)], collapse = "+"), "+",
        paste(colnames(TaxiTrain)[c(6:86)], collapse = "+"),
        sep = ""
    ))

# Clear RAM
gc()

# Use fastLm to train model for Linear Regression
fit <- fastLm(nam, data=TaxiTrain)

# Use model to predict Number of Journeys in the training set
TaxiTrain$Pred_Num_Jrnys <- predict(fit, 
    newdata = TaxiTrain)

# Use model to predict Number of Journeys in the test set
TaxiTest$Pred_Num_Jrnys <- predict(fit, 
    newdata = TaxiTest)

# See output of the Linear Regression model and the main components of it
summary(fit)

# Check how well the model performs on the training set - correlation^2, RMSE and MAE
train.corr <- cor(TaxiTrain$Pred_Num_Jrnys, TaxiTrain$Num_Jrnys)
train.RMSE <- sqrt(mean((TaxiTrain$Pred_Num_Jrnys - TaxiTrain$Num_Jrnys)^2))
train.MAE <- mean(abs(TaxiTrain$Pred_Num_Jrnys - TaxiTrain$Num_Jrnys))
c(train.corr^2, train.RMSE, train.MAE)

# Check how well the model performs on the test set - correlation^2, RMSE and MAE
test.corr <- cor(TaxiTest$Pred_Num_Jrnys, TaxiTest$Num_Jrnys)
test.RMSE <- sqrt(mean((TaxiTest$Pred_Num_Jrnys - TaxiTest$Num_Jrnys)^2))
test.MAE <- mean(abs(TaxiTest$Pred_Num_Jrnys - TaxiTest$Num_Jrnys))
c(test.corr^2, test.RMSE, test.MAE)


