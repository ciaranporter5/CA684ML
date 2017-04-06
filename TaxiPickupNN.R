library(nnet)

# Testing of neural net model on TaxiModel data
# First normalise the data on passenger count and freq
maxTaxi <- apply(TaxiModel[,c(4,6)], 2, max)
minTaxi <- apply(TaxiModel[,c(4,6)], 2, min)

# scale and reassign values to passenger count and freq
scaled.TaxiModel <- as.data.frame(scale(TaxiModel[,c(4,6)],center = minTaxi, 
                                        scale = maxTaxi - minTaxi))

TaxiModel$passenger_count <- scaled.TaxiModel$passenger_count
TaxiModel$freq <- scaled.TaxiModel$freq

# Split based off of split Boolean Vector
TaxiSplit = sample.split(TaxiModel$freq, SplitRatio = 0.70)
TaxiTrain = subset(TaxiModel, split == TRUE)
TaxiTest = subset(TaxiModel, split == FALSE)

# Run the neural net on the training dataset
TaxiNnet <- nnet(TaxiTrain[-c(1,2,3,5,6)], TaxiTrain[6], size =1, 
                 linout = TRUE)

# predict on test data and compare results
TaxiPredict <- predict(TaxiNnet,TaxiTest[-c(1,2,3,5,6)])
head(TaxiPredict)
head(data.frame(TaxiTest$freq,TaxiPredict))

# rescale back to non-normalised and calculate RMSE
TaxiPredictReal <- TaxiPredict*(maxTaxi[2]-minTaxi[2])+minTaxi[2]
TaxiTestReal <- TaxiTest$freq*(maxTaxi[2]-minTaxi[2])+minTaxi[2]
taxi.rmse <- sqrt(mean((TaxiPredictReal- TaxiTestReal)^2))
taxi.rmse