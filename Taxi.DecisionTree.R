install.packages('randomForest')
install.pacakges("geohash")
install.packages("caTools")
install.packages("rpart")
install.packages("ggplot2")


library(randomForest)
library(geohash)
library(caTools)
library(rpart)
library(ggplot2)



setwd("C:/Users/Maeve/Documents/MCM/ML_Test")

# read-in taxi data
TaxiPickupSummary <- read.csv("Summarized_inc_weather_Final.csv")

#random sample without replacement from table
TaxiPickupSummary <- TaxiPickupSummary[sample(1:nrow(TaxiPickupSummary), 100000,
                          replace=FALSE),]
summary(TaxiPickupSummary)

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]

#make model reproducible
set.seed(415)


#include in latitude and longitude information relevative to the geohashes - reverse geocode
TaxiPickupSummary$ReversedLat <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lat
TaxiPickupSummary$ReversedLong <- gh_decode(as.character(TaxiPickupSummary$pickup_Geohash))$lng

#Extract the unique lats and longs
DistinctLat <- unique(TaxiPickupSummary$ReversedLat) 
DistinctLong <-unique(TaxiPickupSummary$ReversedLong)

#check lengths of distinct lats and long to ensure counts are less than 53
length(DistinctLat) #gives 36
length(DistinctLong) # gives 24

#Rank by lat descending and long ascending
RankedLat <- data.frame(DistinctLat, rank(-DistinctLat))
colnames(RankedLat)[1] <- "ReversedLat"
#View(RankedLat)
RankedLong <- data.frame(DistinctLong, rank(DistinctLong))
colnames(RankedLong)[1] <- "ReversedLong"
#View(RankedLong)



#Join back into TaxiPickupSummary table
TaxiData1 <- merge(TaxiPickupSummary, RankedLat, by = "ReversedLat")
TaxiData2 <- merge(TaxiData1, RankedLong, by = "ReversedLong")
colnames(TaxiData2)[33] <- "RankLat"
colnames(TaxiData2)[34] <- "RankLong"


# Split into training and test data based off of split Boolean Vector
TaxiData2$TaxiSplit <- sample.split(
  TaxiData2$Num_Jrnys, SplitRatio = 0.70)
TaxiTest <- subset(TaxiData2, TaxiSplit == FALSE)
TaxiTrain <- subset(TaxiData2, TaxiSplit == TRUE)

# Baseline model - predict the mean of the training data
#best guess for the number of Journeys in the absence of any predictors
best.guess <- mean(TaxiTest$Num_Jrnys) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-TaxiTest$Num_Jrnys)^2))
RMSE.baseline
#gives 85.39

MAE.baseline <- mean(abs(best.guess-TaxiTest$Num_Jrnys))
MAE.baseline
#gives 58.59


#Convert some factor variables to numeric (train and test sets)
TaxiTrain$Temp <- as.numeric(TaxiTrain$Temp)
TaxiTest$Temp <- as.numeric(TaxiTest$Temp)
TaxiTrain$Humidity <- as.numeric(TaxiTrain$Humidity)
TaxiTest$Humidity <- as.numeric(TaxiTest$Humidity)
TaxiTrain$Wind_Speed <- as.numeric(TaxiTrain$Wind_Speed)
TaxiTest$Wind_Speed <- as.numeric(TaxiTest$Wind_Speed)

#There is no need to one-hot encode categorical variables as the R library
#for Random Forest can support categorical variables

# Regression Problem
# Create a random forest with 50 trees
# Update the model using different combinations of variables, run and record results
Taxi_fit_R1 <- randomForest(Num_Jrnys ~ Month + Week_Day + 
                              TimeInterval + RankLat + RankLong + Temp + Precip + Humidity,
                         data=TaxiTrain, 
                         importance=TRUE, 
                         ntree=50)

Taxi_fit_R2 <- randomForest(Num_Jrnys ~ Month + Week_Day + 
                              TimeInterval + RankLat + RankLong + Temp + Precip + Humidity,
                          data=TaxiTrain, 
                          importance=TRUE, 
                          ntree=50)

Taxi_fit_R3 <- randomForest(Num_Jrnys ~ Month + Week_Day + 
                              TimeInterval + RankLat + RankLong + Temp + Precip + Humidity,
                           data=TaxiTrain, 
                           importance=TRUE, 
                           ntree=50)

#Combine seperate models for a better prediction
Taxi_fit_R <- combine(Taxi_fit_R1, Taxi_fit_R2, Taxi_fit_R3)

print(Taxi_fit_R)

# How many trees are needed to reach the minimum error estimate? 
R1_min <- which.min(Taxi_fit_R1$mse)
R2_min <- which.min(Taxi_fit_R2$mse)
R3_min <- which.min(Taxi_fit_R3$mse)

#Generate importance matrix
imp <- as.data.frame(sort(importance(Taxi_fit_R)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# Predict and evaluate on the test set
taxi_model_pred_R <- predict(Taxi_fit_R,TaxiTest)

#Generate root mean square error
RMSE.taxi_model_R <- sqrt(mean((taxi_model_pred_R-TaxiTest$Num_Jrnys)^2))
RMSE.taxi_model_R

#Generate mean absolute error
MAE.taxi_model_R <- mean(abs(taxi_model_pred_R-TaxiTest$Num_Jrnys))
MAE.taxi_model_R


#Generate plot of actual versus predicted
actual <- TaxiTest$Num_Jrnys
predicted <- taxi_model_pred_R
result<-data.frame(actual=actual,predicted=predicted)
paste('Function Call: ', Taxi_fit_R$call)

ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.7)+
  ggtitle('Plotting Error')

#choose a tree from he randomforest to understand results
Sample <- getTree(Taxi_fit_R, k=1, labelVar=TRUE)
Sample

plot(Sample, compress = TRUE)
