# Run Support Vector Machine on test and training data
#install.packages("e1071")
library(e1071)
library(caTools)

# adjust to use 3M sample due to memory consumption of full population
# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary3M <- subset(TaxiPickupSummary,Month == "January"|
                              Month == "February" |Month == "March")
set.seed(3)
TaxiPickupSummary3M$TaxiSplit <- sample.split(
  TaxiPickupSummary3M$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain <- subset(TaxiPickupSummary3M, TaxiSplit == TRUE)
TaxiTest <- subset(TaxiPickupSummary3M, TaxiSplit == FALSE)

# write function that scales data to 0-1 range
minMaxScaling <- function(original, max, min){
  (original - min)/(max - min)
}

# write function to bring back to original for comparing outputs
minMaxDescaling <- function(scaled, max, min){
  ((scaled*(max-min)) + min)
}

# scale the continuous variables in the training dataset (values between 0-1)
TaxiTrain$Num_Jrnys_Scaled <- minMaxScaling(TaxiTrain$Num_Jrnys, max(TaxiTrain$Num_Jrnys),
                                            min(TaxiTrain$Num_Jrnys))

TaxiTrain$total_passenger_count_Scaled <- minMaxScaling(TaxiTrain$total_passenger_count,
                                                        max(TaxiTrain$total_passenger_count),
                                                        min(TaxiTrain$total_passenger_count))

TaxiTrain$total_fare_Scaled <- minMaxScaling(TaxiTrain$total_fare, max(TaxiTrain$total_fare),
                                             min(TaxiTrain$total_fare))

# scale the continuous variables in the test dataset (values between 0-1)
TaxiTest$Num_Jrnys_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys, max(TaxiTest$Num_Jrnys),
                                           min(TaxiTest$Num_Jrnys))

TaxiTest$total_passenger_count_Scaled <- minMaxScaling(TaxiTest$total_passenger_count,
                                                       max(TaxiTest$total_passenger_count),
                                                       min(TaxiTest$total_passenger_count))

TaxiTest$total_fare_Scaled <- minMaxScaling(TaxiTest$total_fare, max(TaxiTest$total_fare),
                                            min(TaxiTest$total_fare))

# Fitting SVM model to training data
set.seed(4)
TaxiSVM <-svm(TaxiTrain[-c(1,2,3,5,6,7,8,9,68,69)], TaxiTrain[69],
              type = "eps-regression", kernel = "radial", scale = FALSE)

# 1. Checks Made on Training Data
# predict on test data and compare results
TaxiSVMPredict <- predict(TaxiSVM,TaxiTrain[-c(1,2,3,5,6,7,8,9,68,69)])
TaxiSVMPredict_DF<- data.frame(TaxiSVMPredict)

# descale the predictions and show inital outputs
TaxiSVMPredict_DF$Descaled <- ifelse(minMaxDescaling(TaxiSVMPredict_DF$TaxiSVMPredict, 
                                                     max(TaxiTrain$Num_Jrnys), 
                                                     min(TaxiTrain$Num_Jrnys))>0,
                                     minMaxDescaling(TaxiSVMPredict_DF$TaxiSVMPredict, 
                                                     max(TaxiTrain$Num_Jrnys), 
                                                     min(TaxiTrain$Num_Jrnys)),0)

head(data.frame(TaxiTrain$Num_Jrnys, TaxiSVMPredict_DF$Descaled),10)

# calculate RMSE on scaled data
taxi.svm_rmse <- sqrt(mean((TaxiSVMPredict_DF$Descaled-TaxiTrain$Num_Jrnys)^2))
taxi.svm_rmse

# calculate Mean Absolute Error on scaled data
taxi.svm_mae <- mean(abs(TaxiSVMPredict_DF$Descaled-TaxiTrain$Num_Jrnys))
taxi.svm_mae

# 2. Checks Made on Test Data
# predict on test data and compare results
TaxiSVMPredict <- predict(TaxiSVM,TaxiTest[-c(1,2,3,5,6,7,8,9,68,69)])
TaxiSVMPredict_DF<- data.frame(TaxiSVMPredict)

# descale the predictions and show inital outputs
TaxiSVMPredict_DF$Descaled<- ifelse(minMaxDescaling(TaxiSVMPredict_DF$TaxiSVMPredict, 
                                                    max(TaxiTest$Num_Jrnys), 
                                                    min(TaxiTest$Num_Jrnys))>0,
                                    minMaxDescaling(TaxiSVMPredict_DF$TaxiSVMPredict, 
                                                    max(TaxiTest$Num_Jrnys), 
                                                    min(TaxiTest$Num_Jrnys)),0)

head(data.frame(TaxiTest$Num_Jrnys, TaxiSVMPredict_DF$Descaled),10)

# calculate RMSE on scaled data
taxi.svm_rmse <- sqrt(mean((TaxiSVMPredict_DF$Descaled-TaxiTest$Num_Jrnys)^2))
taxi.svm_rmse

# calculate Mean Absolute Error on scaled data
taxi.svm_mae <- mean(abs(TaxiSVMPredict_DF$Descaled--TaxiTest$Num_Jrnys))
taxi.svm_mae

# Ouptut Predicted Results
taxi.svm_predicted <- data.frame(TaxiTest$pickup_Geohash, TaxiTest$pickupDate,
                                 TaxiTest$TimeInterval,TaxiTest$Num_Jrnys,
                                 TaxiSVMPredict_DF$Descaled,
                                 TaxiTest$Num_Jrnys - TaxiSVMPredict_DF$Descaled,
                                 abs((TaxiTest$Num_Jrnys - TaxiSVMPredict_DF$Descaled)/
                                       TaxiTest$Num_Jrnys))
colnames(taxi.svm_predicted) <- c("Geohash","Date", "Time", "Original", 
                                  "Predicted", "Difference", "Perc Diff")
# write predicted output to csv file
write.csv(taxi.svm_predicted, file = "TaxiSVMPredicted.csv", row.names = FALSE)
