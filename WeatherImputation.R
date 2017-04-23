#install.packages("readxl")
#install.packages("UsingR")
#install.packages("imputeTS")
#install.packages("missForest")
library(readxl)
library(UsingR)
library(imputeTS)
library(missForest)

#Set the working directory
setwd("C:\\Users\\lvanrheena007\\Documents\\DCU\\CA684 ML")

#read in weather data
weather <- read_excel("Weather_Interim.xlsx", sheet = "Interim")

#Check weather data for missing values and overall distribution
summary(weather)
densityplot(weather)
sum(is.na(weather))

#find missing values - Time Series imputation for records with no values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(weather,1,pMiss)

#find missing values - Random Forest imputation for missing records with some values
weather.mis <- prodNA(weather, noNA = 0.1)
weather.imp <- missForest(weather.mis)

#output imputed weather data
write.csv(weather.imp.$ximp, "Weather_Complete.csv")
