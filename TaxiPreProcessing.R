#install.packages("data.table")
#install.packages("plyr")
#install.packages("caret")
#install.packages("caTools")
#install.pacakges("geohash")
library(data.table)
library(plyr)
library(caret)
library(caTools)
#library(geohash)

# read-in taxi data
TaxiPickupSummary <- read.csv("Raw Data/Summarized_inc_weather_Final.csv")

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]
head(TaxiPickupSummary,1)

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
for (timePeriod in unique(TaxiPickupSummary$TimeInterval)){
  if (as.character(timePeriod) != "00:00"){
    TaxiPickupSummary[as.character(timePeriod)] <- 
      ifelse(TaxiPickupSummary$TimeInterval==timePeriod,1,0)
  }
}

# assign value to categorical variables based on conditions bin
for (currentConditions in unique(TaxiPickupSummary$Conditions)){
  if (currentConditions != "Clear"){
    TaxiPickupSummary[currentConditions] <- 
      ifelse(TaxiPickupSummary$Conditions==currentConditions,1,0)
  }
}

#include in latitude and longitude information relevative to the geohashes
TaxiPickupSummary$ReversedLat <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lat
TaxiPickupSummary$ReversedLong <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lng

#Extract the unique lats and longs
DistinctLat <- unique(TaxiPickupSummary$ReversedLat) 
DistinctLong <-unique(TaxiPickupSummary$ReversedLong)

#Rank by lat descending and long ascending
RankedLat <- data.frame(DistinctLat, rank(-DistinctLat))
colnames(RankedLat)[1] <- "ReversedLat"

RankedLong <- data.frame(DistinctLong, rank(DistinctLong))
colnames(RankedLong)[1] <- "ReversedLong"

#Join back into TaxiPickupSummary table
TaxiPickupSummary <- merge(TaxiPickupSummary, RankedLat, by = "ReversedLat")
TaxiPickupSummary <- merge(TaxiPickupSummary, RankedLong, by = "ReversedLong")
colnames(TaxiPickupSummary)[101] <- "RankLat" # update value to different column if prior dummies not used
colnames(TaxiPickupSummary)[102] <- "RankLong"# update value to different column if prior dummies not used

# Set seed so that same training/test set is used on each run
set.seed(1)

# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary$TaxiSplit <- sample.split(
  TaxiPickupSummary$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain <- subset(TaxiPickupSummary, TaxiSplit == TRUE)
TaxiTest <- subset(TaxiPickupSummary, TaxiSplit == FALSE)

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
