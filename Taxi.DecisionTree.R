#install.packages("data.table")
#install.packages("caTools")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("plyr")
#install.packages("caret")

library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(plyr)
library(caret)


# read-in taxi data
#TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_v3.csv")
#TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_test.csv")
#TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_w_lags_test.csv")
TaxiPickupSummary <- read.csv("Raw Data/Summarized_inc_weather_Final.csv")
#TaxiPickupSummary <- read.csv("Raw Data/Summarized_inc_weather_Final_test.csv")

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]
head(TaxiPickupSummary,1)


# delete redundent columns from table
TaxiPickupSummary$pickupDate <- NULL
TaxiPickupSummary$total_passenger_count <- NULL
TaxiPickupSummary$total_fare <- NULL
#TaxiPickupSummary$TaxiSplit<- NULL

# Set seed so that same training/test set is used on each run
set.seed(1)

# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary$TaxiSplit = sample.split(
  TaxiPickupSummary$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain = subset(TaxiPickupSummary, TaxiSplit == TRUE)
TaxiTest = subset(TaxiPickupSummary, TaxiSplit == FALSE)


# write function that scales data to 0-1 range
minMaxScaling <- function(original, max, min){
  (original - min)/(max - min)
}

# write function to bring back to original for comparing outputs
minMaxDescaling <- function(scaled, max, min){
  ((scaled*(max-min)) + min)
}

# scale the number of journeys (including lags) in the training dataset (values between 0-1)
TaxiTrain$Num_Jrnys_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys, 
                                            max(TaxiTrain$Num_Jrnys),
                                            min(TaxiTrain$Num_Jrnys))

TaxiTrain$Num_Jrnys_Prev_Day_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys_Prev_Day, 
                                                     max(TaxiTrain$Num_Jrnys_Prev_Day),
                                                     min(TaxiTrain$Num_Jrnys_Prev_Day))

TaxiTrain$Num_Jrnys_Prev_Week_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys_Prev_Week, 
                                                      max(TaxiTrain$Num_Jrnys_Prev_Week),
                                                      min(TaxiTrain$Num_Jrnys_Prev_Week))

TaxiTrain$Num_Jrnys_Prev_Hour_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys_Prev_Hour,
                                                      max(TaxiTrain$Num_Jrnys_Prev_Hour),
                                                      min(TaxiTrain$Num_Jrnys_Prev_Hour))

TaxiTrain$Num_Jrnys_Prev_HalfHour_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys_Prev_HalfHour,
                                                          max(TaxiTrain$Num_Jrnys_Prev_HalfHour),
                                                          min(TaxiTrain$Num_Jrnys_Prev_HalfHour))

# scale the number of journeys (including lags) in the test dataset (values between 0-1)
TaxiTest$Num_Jrnys_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys, 
                                           max(TaxiTest$Num_Jrnys),
                                           min(TaxiTest$Num_Jrnys))

TaxiTest$Num_Jrnys_Prev_Day_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys_Prev_Day, 
                                                    max(TaxiTest$Num_Jrnys_Prev_Day),
                                                    min(TaxiTest$Num_Jrnys_Prev_Day))

TaxiTest$Num_Jrnys_Prev_Week_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys_Prev_Week, 
                                                     max(TaxiTest$Num_Jrnys_Prev_Week),
                                                     min(TaxiTest$Num_Jrnys_Prev_Week))

TaxiTest$Num_Jrnys_Prev_Hour_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys_Prev_Hour,
                                                     max(TaxiTest$Num_Jrnys_Prev_Hour),
                                                     min(TaxiTest$Num_Jrnys_Prev_Hour))

TaxiTest$Num_Jrnys_Prev_HalfHour_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys_Prev_HalfHour,
                                                         max(TaxiTest$Num_Jrnys_Prev_HalfHour),
                                                         min(TaxiTest$Num_Jrnys_Prev_HalfHour))



# Checking amount of records in each table
dim(TaxiPickupSummary)
dim(TaxiTrain)
dim(TaxiTest)

taxiTestmax <- TaxiTest$Num_Jrnys
taxiTrainmax <- TaxiTrain$Num_Jrnys

# Deleting TaxiSplit column from test and training dataset
TaxiTest$TaxiSplit <- NULL
TaxiTest$DAY <- NULL
TaxiTest$Num_Jrnys <- NULL
TaxiTest$Num_Jrnys_Prev_Day <- NULL
TaxiTest$Num_Jrnys_Prev_Week <- NULL
TaxiTest$Num_Jrnys_Prev_Hour <- NULL
TaxiTest$Num_Jrnys_Prev_HalfHour <- NULL
TaxiTrain$TaxiSplit <- NULL
TaxiTrain$DAY <- NULL
TaxiTrain$Num_Jrnys <- NULL
TaxiTrain$Num_Jrnys_Prev_Day <- NULL
TaxiTrain$Num_Jrnys_Prev_Week <- NULL
TaxiTrain$Num_Jrnys_Prev_Hour <- NULL
TaxiTrain$Num_Jrnys_Prev_HalfHour <- NULL


#dtm <- rpart (Num_Jrnys_Scaled~., TaxiTrain, method="class")
#dtm <- rpart (Num_Jrnys_Scaled~., TaxiTrain, method="anova")
dtm <- rpart (Num_Jrnys_Scaled ~ Week_Day+Month+TimeInterval+pickup_Geohash+Temp+Wind_Speed+Precip+Conditions, TaxiTrain, method="anova")
#dtm <- rpart (Num_Jrnys ~ TimeInterval+DAY, TaxiTrain, method="class")
#dtn <- rpart (pickup_Geohash~., TaxiTrain, method="anova")
#dtn <- rpart (pickup_Geohash~timeInterval+day, TaxiTrain, method="class")

dtm
plot(dtm)
text(dtm)
rpart.plot(dtm)
p <- predict (dtm, TaxiTrain)

pp <- data.frame(p)


# descale the predictions and show inital outputs
pp$Descaled <- ifelse(minMaxDescaling(pp$p, max(taxiTrainmax),
                                      min(taxiTrainmax))>0,
                      minMaxDescaling(pp$p,
                                      max(taxiTrainmax),
                                      min(taxiTrainmax)),0)

head(data.frame(taxiTrainmax, pp$Descaled),10)


printcp(dtm)
plotcp(dtm)
dtm_prune <-prune.rpart(dtm, cp=0.024)
dtm_prune

p <- predict (dtm_prune, TaxiTest)

pp <- data.frame(p)


# descale the predictions and show inital outputs
pp$Descaled <- ifelse(minMaxDescaling(pp$p, max(taxiTestmax),
                                      min(taxiTestmax))>0,
                      minMaxDescaling(pp$p,
                                      max(taxiTestmax),
                                      min(taxiTestmax)),0)

head(data.frame(taxiTestmax, pp$Descaled),100)


# calculate RMSE on scaled data
taxi.nn_rmse <- sqrt(mean((pp$Descaled-taxiTestmax)^2))
taxi.nn_rmse

# calculate Mean Absolute Error on scaled data
taxi.nn_mae <- mean(abs(pp$Descaled-taxiTestmax))
taxi.nn_mae

