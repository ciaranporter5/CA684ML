# Run Support Vector Machine on test and training data
#install.packages("e1071")
library(e1071)
library(caTools)

# adjust to 1 month training/test split due to long run time
# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary1M <- subset(TaxiPickupSummary,Month == "January")

set.seed(3)
TaxiPickupSummary1M$TaxiSplit <- sample.split(
  TaxiPickupSummary1M$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain <- subset(TaxiPickupSummary1M, TaxiSplit == TRUE)
TaxiTest <- subset(TaxiPickupSummary1M, TaxiSplit == FALSE)

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

# scale the continuous variables in the test dataset (values between 0-1)
TaxiTest$Num_Jrnys_Scaled <- minMaxScaling(TaxiTest$Num_Jrnys, max(TaxiTest$Num_Jrnys),
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

# Fitting model to training data
set.seed(4)

TaxiSVM <-svm(
  TaxiTrain[c(17,25,27,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
              56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
              80,81,82,83,84,85,86,87,101,102,105,106,107,108)],
              TaxiTrain$Num_Jrnys_Scaled, type = "eps-regression", 
              kernel = "linear", scale = FALSE)

# 1. Checks Made on Training Data
# predict on test data and compare results
TaxiSVMPredict <- 
  predict(TaxiSVM,
          TaxiTrain[c(17,25,27,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
                      56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
                      80,81,82,83,84,85,86,87,101,102,105,106,107,108)])
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
TaxiSVMPredict <- 
  predict(TaxiSVM,
          TaxiTest[c(17,25,27,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
                     56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
                     80,81,82,83,84,85,86,87,101,102,105,106,107,108)])
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

taxi.svm_predicted <- data.frame(TaxiTest$pickup_Geohash, TaxiTest$pickupDate,
                                 TaxiTest$TimeInterval,TaxiTest$Num_Jrnys,
                                 TaxiSVMPredict_DF$Descaled,
                                 TaxiTest$Num_Jrnys - TaxiSVMPredict_DF$Descaled,
                                 abs((TaxiTest$Num_Jrnys - TaxiSVMPredict_DF$Descaled)/
                                       TaxiTest$Num_Jrnys))
colnames(taxi.svm_predicted) <- c("Geohash","Date", "Time", "Original", 
                                  "Predicted", "Difference", "Perc Diff")
write.csv(taxi.svm_predicted, file = "TaxiSVMPredicted.csv", row.names = FALSE)
