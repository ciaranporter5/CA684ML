library(geohash)
library(leaflet)
library(data.table)
library(plyr)
library(caret)
library(caTools)

# read-in taxi data
TaxiPickupSummary <- read.csv("Raw Data/Taxi_Summarized_Pickup.csv")
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "NULL",]
head(TaxiPickupSummary)

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

# assign value to categorical variables for payment types
# TaxiModel$CreditCard <- 0
# TaxiModel$Cash <- 0
# TaxiModel$NoCharge <- 0
# TaxiModel$Dispute <- 0
# TaxiModel$Unknown <- 0
# 
# TaxiModel[TaxiModel$payment_type == 1, "CreditCard"] <- 1
# TaxiModel[TaxiModel$payment_type == 2, "Cash"] <- 1
# TaxiModel[TaxiModel$payment_type == 3, "NoCharge"] <- 1
# TaxiModel[TaxiModel$payment_type == 4, "Dispute"] <- 1
# TaxiModel[TaxiModel$payment_type == 5, "Unknown"] <- 1
