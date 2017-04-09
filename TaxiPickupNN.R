library(data.table)
library(plyr)
library(caret)
library(caTools)
library(nnet)

# Running of neural net model on Taxi Pickup data
set.seed(1)

# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary$TaxiSplit = sample.split(
  TaxiPickupSummary$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain = subset(TaxiPickupSummary, TaxiSplit == TRUE)
TaxiTest = subset(TaxiPickupSummary, TaxiSplit == FALSE)

# scale the continuous variables in the training/test dataset (mean=0, sd=1)
TaxiTrain$Num_Jrnys_Scaled <- scale(TaxiTrain$Num_Jrnys)
TaxiTrain$total_passenger_count_Scaled<- scale(TaxiTrain$total_passenger_count)
TaxiTrain$total_fare_Scaled <- scale(TaxiTrain$total_fare)

TaxiTest$Num_Jrnys_Scaled <- scale(TaxiTest$Num_Jrnys)
TaxiTest$total_passenger_count_Scaled<- scale(TaxiTest$total_passenger_count)
TaxiTest$total_fare_Scaled <- scale(TaxiTest$total_fare)

TaxiTestScaleCompare <- as.data.frame(TaxiTest[c(7,69)])
#write.csv(TaxiTestScaleCompare, file = "TaxiTestScale.csv", row.names = FALSE)

# Run the neural net on the scaled training dataset
TaxiNnet <- nnet(TaxiTrain[-c(1,2,3,5,6,7,8,9,68,69)], TaxiTrain[69], size =1, 
                 linout = TRUE, maxit=130)

# predict on test data and compare results
TaxiPredict <- predict(TaxiNnet,TaxiTest[-c(1,2,3,5,6,7,8,9,68,69)])
head(TaxiPredict)
head(data.frame(TaxiTest$Num_Jrnys_Scaled,TaxiPredict))

# calculate RMSE on scaled data
taxi.nn_rmse <- sqrt(mean((TaxiPredict-TaxiTest$Num_Jrnys_Scaled)^2))
taxi.nn_rmse

# calculate Mean Absolute Error on scaled data
taxi.nn_mae <- mean(abs(TaxiPredict-TaxiTest$Num_Jrnys_Scaled))
taxi.nn_mae

taxi.nn_predicted <- data.frame(TaxiTest$Num_Jrnys_Scaled,TaxiPredict,
                                TaxiTest$Num_Jrnys_Scaled - TaxiPredict,
                                abs((TaxiTest$Num_Jrnys_Scaled - TaxiPredict)/
                                  TaxiTest$Num_Jrnys_Scaled))
colnames(taxi.nn_predicted) <- c("Original", "Predicted", "Difference", "Perc Diff")
write.csv(taxi.nn_predicted, file = "TaxiNNPredicted.csv", row.names = FALSE)

