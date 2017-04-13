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
TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized by pickup location_w_lags.csv")

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





# include in latitude and longitude information relevative to the geohashes
#TaxiTrain$ReversedLat <- gh_decode(as.character(TaxiTrain$pickup_Geohash))$lat
#TaxiTrain$ReversedLong <- gh_decode(as.character(TaxiTrain$pickup_Geohash))$lng
#TaxiTest$ReversedLat <- gh_decode(as.character(TaxiTest$pickup_Geohash))$lat
#TaxiTest$ReversedLong <- gh_decode(as.character(TaxiTest$pickup_Geohash))$lng
