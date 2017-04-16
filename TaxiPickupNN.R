# Run Neural Network on Training Data and Test Data
# Check error on training data using Root Mean Squared Error (RMSE) and Mean Absolute Error (MAE). 
# If this is too low on training, risk of overfitting.
# There should be some element of error in the training set,
# but the test set error should not be much higher for model to be reasonable accurate.

#install.packages("nnet")
library(data.table)
library(plyr)
library(caret)
library(caTools)
library(nnet)

# Run the neural net on the scaled training dataset
# check if convergence occurs or need to introduce max iterations
# Run set seed each time with nnet as will use random inputs each time otherwise
set.seed(10)
TaxiNnet <- nnet(TaxiTrain[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                              22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                              94,95,96,97,98,99,100,103,104)], 
                 TaxiTrain$Num_Jrnys_Scaled, maxit =200, decay=0.01,
                 size =1,linout = TRUE)


# 1. Checks Made on Training Data
# predict on test data and compare results
TaxiPredict <- predict(TaxiNnet,TaxiTrain[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                             22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                                             94,95,96,97,98,99,100,103,104)])
TaxiPredict_DF<- data.frame(TaxiPredict)

# descale the predictions and show inital outputs
TaxiPredict_DF$Descaled <- ifelse(minMaxDescaling(TaxiPredict_DF$TaxiPredict, 
                                        max(TaxiTrain$Num_Jrnys), 
                                        min(TaxiTrain$Num_Jrnys))>0,
                                  minMaxDescaling(TaxiPredict_DF$TaxiPredict, 
                                                  max(TaxiTrain$Num_Jrnys), 
                                                  min(TaxiTrain$Num_Jrnys)),0)

head(data.frame(TaxiTrain$Num_Jrnys, TaxiPredict_DF$Descaled),100)

# calculate RMSE on scaled data
  taxi.nn_rmse <- sqrt(mean((TaxiPredict_DF$Descaled-TaxiTrain$Num_Jrnys)^2))
  taxi.nn_rmse

# calculate Mean Absolute Error on scaled data
taxi.nn_mae <- mean(abs(TaxiPredict_DF$Descaled-TaxiTrain$Num_Jrnys))
taxi.nn_mae

# 2. Checks Made on Test Data
# predict on test data and compare results
TaxiPredict <- predict(TaxiNnet,TaxiTest[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                            22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                                            94,95,96,97,98,99,100,103,104)])
TaxiPredict_DF<- data.frame(TaxiPredict)

# descale the predictions and show inital outputs
TaxiPredict_DF$Descaled <- ifelse(minMaxDescaling(TaxiPredict_DF$TaxiPredict, 
                                                  max(TaxiTest$Num_Jrnys), 
                                                  min(TaxiTest$Num_Jrnys))>0,
                                  minMaxDescaling(TaxiPredict_DF$TaxiPredict, 
                                                  max(TaxiTest$Num_Jrnys), 
                                                  min(TaxiTest$Num_Jrnys)),0)

head(data.frame(TaxiTest$Num_Jrnys, TaxiPredict_DF$Descaled),100)

# calculate RMSE on scaled data
taxi.nn_rmse <- sqrt(mean((TaxiPredict_DF$Descaled-TaxiTest$Num_Jrnys)^2))
taxi.nn_rmse

# calculate Mean Absolute Error on scaled data
taxi.nn_mae <- mean(abs(TaxiPredict_DF$Descaled-TaxiTest$Num_Jrnys))
taxi.nn_mae

taxi.nn_predicted <- data.frame(TaxiTest$pickup_Geohash, TaxiTest$pickupDate,
                                TaxiTest$TimeInterval,TaxiTest$Num_Jrnys,
                                TaxiPredict_DF$Descaled,
                                TaxiTest$Num_Jrnys - TaxiPredict_DF$Descaled,
                                abs((TaxiTest$Num_Jrnys - TaxiPredict_DF$Descaled)/
                                  TaxiTest$Num_Jrnys))
colnames(taxi.nn_predicted) <- c("Geohash","Date", "Time", "Original", 
                                 "Predicted", "Difference", "Perc Diff")
write.csv(taxi.nn_predicted, file = "TaxiNNPredictedv2.csv", row.names = FALSE)

# 3. Test the avNNet
NumJrnysScaledUnlisted <- unlist(TaxiTrain[104])
set.seed(25)
TaxiAvNNet <- avNNet(TaxiTrain[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                  22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                                  94,95,96,97,98,99,100,103,104)], 
                     NumJrnysScaledUnlisted,linout = TRUE, size =1, repeats = 10)


# A. Checks Made on Training Data
# predict on test data and compare results
TaxiAvNNPredict <- predict(TaxiAvNNet,TaxiTrain[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                                   22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                                                   94,95,96,97,98,99,100,103,104)])
TaxiAvNNPredict_DF<- data.frame(TaxiAvNNPredict)

# descale the predictions and show inital outputs
TaxiAvNNPredict_DF$Descaled <- ifelse(minMaxDescaling(TaxiAvNNPredict_DF$TaxiAvNNPredict, 
                                                  max(TaxiTrain$Num_Jrnys), 
                                                  min(TaxiTrain$Num_Jrnys))>0,
                                  minMaxDescaling(TaxiAvNNPredict_DF$TaxiAvNNPredict, 
                                                  max(TaxiTrain$Num_Jrnys), 
                                                  min(TaxiTrain$Num_Jrnys)),0)

head(data.frame(TaxiTrain$Num_Jrnys, TaxiAvNNPredict_DF$Descaled),10)

# calculate RMSE on scaled data
taxi.avnn_rmse <- sqrt(mean((TaxiAvNNPredict_DF$Descaled-TaxiTrain$Num_Jrnys)^2))
taxi.avnn_rmse

# calculate Mean Absolute Error on scaled data
taxi.avnn_mae <- mean(abs(TaxiAvNNPredict_DF$Descaled-TaxiTrain$Num_Jrnys))
taxi.avnn_mae

# B. Checks Made on Test Data
# predict on test data and compare results
TaxiAvNNPredict <- predict(TaxiAvNNet,TaxiTest[-c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                                                  22,23,24,25,26,27,28,29,88,89,90,91,92,93,
                                                  94,95,96,97,98,99,100,103,104)])
TaxiAvNNPredict_DF<- data.frame(TaxiAvNNPredict)

# descale the predictions and show inital outputs
TaxiAvNNPredict_DF$Descaled <- ifelse(minMaxDescaling(TaxiAvNNPredict_DF$TaxiAvNNPredict, 
                                                  max(TaxiTest$Num_Jrnys), 
                                                  min(TaxiTest$Num_Jrnys))>0,
                                  minMaxDescaling(TaxiAvNNPredict_DF$TaxiAvNNPredict, 
                                                  max(TaxiTest$Num_Jrnys), 
                                                  min(TaxiTest$Num_Jrnys)),0)

head(data.frame(TaxiTest$Num_Jrnys, TaxiAvNNPredict_DF$Descaled),10)

# calculate RMSE on scaled data
taxi.avnn_rmse <- sqrt(mean((TaxiAvNNPredict_DF$Descaled-TaxiTest$Num_Jrnys)^2))
taxi.avnn_rmse

# calculate Mean Absolute Error on scaled data
taxi.avnn_mae <- mean(abs(TaxiAvNNPredict_DF$Descaled-TaxiTest$Num_Jrnys))
taxi.avnn_mae

taxi.avnn_predicted <- data.frame(TaxiTest$pickup_Geohash, TaxiTest$pickupDate,
                                TaxiTest$TimeInterval,TaxiTest$Num_Jrnys,
                                TaxiAvNNPredict_DF$Descaled,
                                TaxiTest$Num_Jrnys - TaxiAvNNPredict_DF$Descaled,
                                abs((TaxiTest$Num_Jrnys - TaxiAvNNPredict_DF$Descaled)/
                                      TaxiTest$Num_Jrnys))
colnames(taxi.avnn_predicted) <- c("Geohash","Date", "Time", "Original", 
                                 "Predicted", "Difference", "Perc Diff")
