#install.pacakges("geohash")
#install.packages("caTools")
#install.packages("rpart")
#install.packages("rpart.plot")

library(geohash)
library(caTools)
library(rpart)
library(rpart.plot)

# read-in taxi data
# commented out versions are old datasets
#TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_v3.csv")
#TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_w_lags.csv")
TaxiPickupSummary <- read.csv("Raw Data/Summarized_inc_weather_Final.csv")

# brief summary of dataframe
summary(TaxiPickupSummary)

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]
head(TaxiPickupSummary,1)

# delete redundent columns from table that are not needed as predictors
TaxiPickupSummary$pickupDate <- NULL
TaxiPickupSummary$total_passenger_count <- NULL
TaxiPickupSummary$total_fare <- NULL
# show first recorded of updated table
head(TaxiPickupSummary,1)

# include in latitude and longitude information relevative to the geohashes
TaxiPickupSummary$ReversedLat <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lat
TaxiPickupSummary$ReversedLong <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lng

# Extract the unique lats and longs
DistinctLat <- unique(TaxiPickupSummary$ReversedLat) 
DistinctLong <-unique(TaxiPickupSummary$ReversedLong)

# view distinct lat & long
View(DistinctLat)
View(DistinctLong)

# check lengths of distinct lats and long to ensure counts are less than 53
length(DistinctLat)
length(DistinctLong)

# Rank by lat descending and long ascending
RankedLat <- data.frame(DistinctLat, rank(-DistinctLat))
colnames(RankedLat)[1] <- "ReversedLat"
#View(RankedLat)
RankedLong <- data.frame(DistinctLong, rank(DistinctLong))
colnames(RankedLong)[1] <- "ReversedLong"
#View(RankedLong)

# join back into TaxiPickupSummary table
TaxiDataUpdated <- merge(TaxiPickupSummary, RankedLat, by = "ReversedLat")
TaxiDataFinal <- merge(TaxiDataUpdated, RankedLong, by = "ReversedLong")
# renameing column names 27 & 28 refer to cols being renamed
colnames(TaxiDataFinal)[27] <- "RankLat"
colnames(TaxiDataFinal)[28] <- "RankLong"

# Set seed so that same training/test set is used on each run
set.seed(9850)

# Split into training and test data based off of split Boolean Vector
TaxiDataFinal$TaxiSplit = sample.split(
  TaxiDataFinal$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain = subset(TaxiDataFinal, TaxiSplit == TRUE)
TaxiTest = subset(TaxiDataFinal, TaxiSplit == FALSE)

# checking record count to see if split was accurate
dim(TaxiDataFinal)
dim(TaxiTrain)
dim(TaxiTest)
summary(TaxiDataFinal,1)
summary(TaxiTrain,1)

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


# Deleting columns from test and training dataset that are not needed
TaxiTest$TaxiSplit <- NULL
TaxiTest$DAY <- NULL
TaxiTest$Num_Jrnys_Prev_Day <- NULL
TaxiTest$Num_Jrnys_Prev_Week <- NULL
TaxiTest$Num_Jrnys_Prev_Hour <- NULL
TaxiTest$Num_Jrnys_Prev_HalfHour <- NULL
TaxiTrain$TaxiSplit <- NULL
TaxiTrain$DAY <- NULL
TaxiTrain$Num_Jrnys_Prev_Day <- NULL
TaxiTrain$Num_Jrnys_Prev_Week <- NULL
TaxiTrain$Num_Jrnys_Prev_Hour <- NULL
TaxiTrain$Num_Jrnys_Prev_HalfHour <- NULL

# summary of training dataset to see fields for use in decesion tree
summary(TaxiTrain)


# grow the decision tree using Num_Jrnys_Scaled as the outcome/target All other variables are predictors in the tree.
# anova is specified when we want to use a regression tree/when our target is numerical

# first model we produced using pickup_Geohash instead of lat & long
taxi_DT_model <- rpart (Num_Jrnys_Scaled ~ Week_Day+Month+TimeInterval+
                          pickup_Geohash+Temp+Wind_Speed+Precip+Conditions,
                        TaxiTrain, method="anova")

# second model we produced using lat & long instead of pickup_Geo_Hash
taxi_DT_model <- rpart (Num_Jrnys_Scaled ~ Week_Day+Month+TimeInterval+
                          RankLat+RankLong+Temp+Wind_Speed+Precip+Conditions, 
                        TaxiTrain, method="anova")

# third model we produced using pickup_Geohash instead of lat & long with only previous journy info and no weather data
taxi_DT_model <- rpart (Num_Jrnys_Scaled ~ pickup_Geohash+Num_Jrnys_Prev_Day_Scaled+
                          Num_Jrnys_Prev_Week_Scaled+Num_Jrnys_Prev_Hour_Scaled+
                          Num_Jrnys_Prev_HalfHour_Scaled, 
                        TaxiTrain, method="anova")

# fourth model we produced using lat & long instead of pickup_GEOhash with with weather data and prev week data
taxi_DT_model <- rpart (Num_Jrnys_Scaled ~ Month+Week_Day+TimeInterval+
                          RankLat+RankLong+Temp+Conditions+Precip+Humidity+
                          Num_Jrnys_Prev_Week_Scaled,
                        TaxiTrain, method="anova")

# fifth model we produced using lat & long with previous journey info and weather data
taxi_DT_model <- rpart (Num_Jrnys_Scaled ~ RankLat+RankLong+Num_Jrnys_Prev_Day_Scaled+
                          Num_Jrnys_Prev_Week_Scaled+Num_Jrnys_Prev_Hour_Scaled+
                          Num_Jrnys_Prev_HalfHour_Scaled+Temp+Wind_Speed+Precip+Conditions, 
                        TaxiTrain, method="anova")


# shows what is in our tree. Gives idea how the tree structure is built, the legend of the trre, the number of observations, branches, where leave nodes are located etc.
taxi_DT_model

# draws decision tree with text. Gives good visualisations for conditions used. Shows primary conditions etc
# type of 4 labels all nodes in the tree, extra of 101 displays % of nodes that fall in a that class
# some models are better suited when no type or extras are specified
rpart.plot(taxi_DT_model, type=4, extra=101)
rpart.plot(taxi_DT_model, type=4)
rpart.plot(taxi_DT_model, extra=101)
rpart.plot(taxi_DT_model)

# plotting the overall complexity parameters and then pruning out the tree to get best fit
printcp(taxi_DT_model)
plotcp(taxi_DT_model)
taxi_DT_model_prune <-prune.rpart(taxi_DT_model, cp=0.07)
taxi_DT_model_prune
printcp(taxi_DT_model_prune)
plotcp(taxi_DT_model_prune)

# get predicted from test data set using pruned & unpruned models
pred <- predict(taxi_DT_model, TaxiTest)
pred <- data.frame(pred)
pred_prune <- predict(taxi_DT_model_prune, TaxiTest)
pred_prune <- data.frame(pred_prune)

summary(pred)
summary(pred_prune)

# descale the predictions and show inital outputs
pred$Descaled <- ifelse(minMaxDescaling(pred$pred, max(TaxiTest$Num_Jrnys),
                                        min(TaxiTest$Num_Jrnys))>0,
                        minMaxDescaling(pred$pred,
                                        max(TaxiTest$Num_Jrnys),
                                        min(TaxiTest$Num_Jrnys)),0)

head(data.frame(TaxiTest$Num_Jrnys, pred$Descaled),100)

# descale the predictions and show inital outputs for pruned data
pred_prune$Descaled <- ifelse(minMaxDescaling(pred_prune$pred, max(TaxiTest$Num_Jrnys),
                                        min(TaxiTest$Num_Jrnys))>0,
                        minMaxDescaling(pred_prune$pred,
                                        max(TaxiTest$Num_Jrnys),
                                        min(TaxiTest$Num_Jrnys)),0)

head(data.frame(TaxiTest$Num_Jrnys, pred_prune$Descaled),100)


# calculate RMSE on scaled data which is has not been pruned
taxi.nn_rmse <- sqrt(mean((pred$Descaled-TaxiTest$Num_Jrnys)^2))
taxi.nn_rmse

# calculate RMSE on scaled data which has been pruned
taxi.nn_rmse <- sqrt(mean((pred_prune$Descaled-TaxiTest$Num_Jrnys)^2))
taxi.nn_rmse

# calculate Mean Absolute Error on scaled data which is has not been pruned
taxi.nn_mae <- mean(abs(pred$Descaled-TaxiTest$Num_Jrnys))
taxi.nn_mae

# calculate Mean Absolute Error on scaled data which has been pruned
taxi.nn_mae <- mean(abs(pred_prune$Descaled-TaxiTest$Num_Jrnys))
taxi.nn_mae
