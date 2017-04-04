library(geohash)
library(leaflet)
library(data.table)
library(plyr)
library(caret)
library(caTools)
library(nnet)

# read-in taxi data
TaxiSample <- read.csv("Raw Data/TaxiNYCSample.csv")
head(TaxiSample)

# split the time stamp into 30 minute bins using function
timebinning <- function(pickup_time){
  paste(substr(pickup_time,1,3),
        sprintf("%02d", floor(as.integer(
          substr(as.character(pickup_time),4,5))/30)*30), sep = "")
}

TaxiSample$time_bin <- timebinning(TaxiSample$pickup_time)

# Extract out date field on its own (DD/MM/YY)
TaxiSample$pickup_date <- substring(TaxiSample$pickup_datetime,1,8)
TaxiSample$dropoff_date <- substring(TaxiSample$dropoff_datetime,1,8)

# Summarise the existing data by pick-up geohash, date (day-bin, tim-bin), 
# number of passengers, weather (later)
TaxiModel <- as.data.frame(count(TaxiSample, 
                                 c("PickupGeohash", "pickup_date","pickup_day_of_week","time_bin",
                                   'passenger_count','payment_type')))

# set up binary variables for days for the week.
# Note that there are 6 variables. Sunday is assumed to be where all 6 equal 0

for (day in unique(TaxiModel$pickup_day_of_week)){
  if (day != "Sun"){
    TaxiModel[day] <- ifelse(TaxiModel$pickup_day_of_week==day,1,0)
  }
}

# assign value to categorical variables based on time period bin
for (timePeriod in unique(TaxiModel$time_bin)){
  if (timePeriod != "00:00"){
    TaxiModel[timePeriod] <- 
      ifelse(TaxiModel$time_bin==timePeriod,1,0)
  }
}

# assign value to categorical variables for payment types
TaxiModel$CreditCard <- 0
TaxiModel$Cash <- 0
TaxiModel$NoCharge <- 0
TaxiModel$Dispute <- 0
TaxiModel$Unknown <- 0

TaxiModel[TaxiModel$payment_type == 1, "CreditCard"] <- 1
TaxiModel[TaxiModel$payment_type == 2, "Cash"] <- 1
TaxiModel[TaxiModel$payment_type == 3, "NoCharge"] <- 1
TaxiModel[TaxiModel$payment_type == 4, "Dispute"] <- 1
TaxiModel[TaxiModel$payment_type == 5, "Unknown"] <- 1
