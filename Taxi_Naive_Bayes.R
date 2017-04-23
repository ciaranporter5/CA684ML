#Set the working directory
setwd("C:/Users/Maeve/Documents/MCM/ML_Test")

#install.packages("e1071")
#install.packages("caTools")
#install.packages ("plyr")
library("e1071")
library("caTools")
library("plyr")


#read in taxi data
TaxiPickupSummary <- read.csv("Summarized_inc_weather_Final.CSV")

# remove blank geohashes - bad records/outside test range
TaxiPickupSummary <- TaxiPickupSummary[TaxiPickupSummary$pickup_Geohash != "",]

#make model reproducible
set.seed(415)

#Check assumptions of Naive Bayes

#1. Normality of continuous features
#Q-Q plots
qqnorm(TaxiPickupSummary$Temp)
qqnorm(TaxiPickupSummary$Precip)
qqnorm(TaxiPickupSummary$Wind_Speed)
qqnorm(TaxiPickupSummary$Humidity)

#Histograms
hist(TaxiPickupSummary$Precip)
#Precip is right skewed as it is often not raining

hist(TaxiPickupSummary$Temp)
#Temp is left skewed as colder temps are rare (those tending towards 0 F)

hist(TaxiPickupSummary$Wind_Speed)
#Temp is right skewed as the wind is often low

#Kolmogorov-Smirnov test
ks.test(TaxiPickupSummary$Temp, "pnorm", mean(TaxiPickupSummary$Temp),
        sd(TaxiPickupSummary$Temp))

ks.test(TaxiPickupSummary$Precip, "pnorm", mean(TaxiPickupSummary$Precip),
        sd(TaxiPickupSummary$Precip))

ks.test(TaxiPickupSummary$Wind_Speed, "pnorm", mean(TaxiPickupSummary$Wind_Speed),
        sd(TaxiPickupSummary$Wind_Speed))

ks.test(TaxiPickupSummary$Humidity, "pnorm", mean(TaxiPickupSummary$Humidity),
        sd(TaxiPickupSummary$Humidity))

# Above tests indicated that continuous variables are non-Gaussian
# Convert the continuous variables to a categorical variables

TaxiPickupSummary$pickup_level <- with(TaxiPickupSummary, cut(Num_Jrnys, 
                                                              breaks=unique(quantile(Num_Jrnys, probs=seq(0,1, by=0.25)), na.rm=TRUE), 
                                                              include.lowest=TRUE, labels = c("low","medium","high","very-high")))

table(TaxiPickupSummary$pickup_level)

TaxiPickupSummary$pickup_level_PrevWk <- with(TaxiPickupSummary, cut(Num_Jrnys_Prev_Week, 
                                                                     breaks=unique(quantile(Num_Jrnys_Prev_Week, probs=seq(0,1, by=0.25)), na.rm=TRUE), 
                                                                     include.lowest=TRUE, labels = c("low","medium","high","very-high")))

table(TaxiPickupSummary$pickup_level_PrevWk)

TaxiPickupSummary$Precip_level<-ifelse(TaxiPickupSummary$Precip ==0,"1",
                                       ifelse(TaxiPickupSummary$Precip < mean(TaxiPickupSummary$Precip[TaxiPickupSummary$Precip!=0]) 
                                              ,"2","3"))

table(TaxiPickupSummary$Precip_level)


TaxiPickupSummary$Temp_level <- with(TaxiPickupSummary, cut(Temp, 
                                                            breaks=unique(quantile(Temp, probs=seq(0,1, by=0.1)), na.rm=TRUE), 
                                                            include.lowest=TRUE, labels = c("1","2","3","4","5","6","7","8","9","10")))

table(TaxiPickupSummary$Temp_level)

TaxiPickupSummary$Windspeed_level<-ifelse(TaxiPickupSummary$Wind_Speed ==0,"1",
                                          ifelse(TaxiPickupSummary$Wind_Speed < mean(TaxiPickupSummary$Wind_Speed[TaxiPickupSummary$Wind_Speed!=0]) 
                                                 ,"2","3"))
table(TaxiPickupSummary$Windspeed_level)


TaxiPickupSummary$Humidity_Level <- with(TaxiPickupSummary, cut(Humidity, 
                                                                breaks=unique(quantile(Humidity, probs=seq(0,1, by=0.25)), na.rm=TRUE), 
                                                                include.lowest=TRUE, labels = c("1","2","3","4")))

table(TaxiPickupSummary$Humidity_Level)



#2. Check Independence of categorical features

#Extract Categorical Variables from main data frame
CatVariables <- TaxiPickupSummary[,c(1,2,3,5,27,29,30,31,32,33)]


#Chi square test for independence - this will generate a matrix of results showing all combinations of categorical variables
#Null hypothesis - variables are independent given the pickup level

combos <- combn(ncol( CatVariables),2)

adply(combos, 2, function(x) {
  test <- chisq.test(CatVariables[, x[1]], CatVariables[, x[2]])
  
  out <- data.frame("Row" = colnames(CatVariables)[x[1]]
                    , "Column" = colnames(CatVariables[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
  )
  return(out)
  
}) 

# Split into training and test data based off of split Boolean Vector
TaxiPickupSummary$TaxiSplit = sample.split(
  TaxiPickupSummary$Num_Jrnys, SplitRatio = 0.70)
TaxiTrain = subset(TaxiPickupSummary, TaxiSplit == TRUE)
TaxiTest = subset(TaxiPickupSummary, TaxiSplit == FALSE)

#Generate tables for testing feature combinations
#Table 1 = Weekday, Month, Time Interval, Pickup Geohash
TaxiTable_train1 <- TaxiTrain[,c(2,3,5,6,28)]
TaxiTable_test1 <- TaxiTest[,c(2,3,5,6,28)]

#Table 2 = Weekday, Month, Time Interval, Pickup Geohash, pickup level prev week
TaxiTable_train2 <- TaxiTrain[,c(2,3,5,6,28,29)]
TaxiTable_test2 <- TaxiTest[,c(2,3,5,6,28,29)]

#Table 3 = Weekday, Month, Time Interval, Pickup Geohash, pickup level prev week, temp_level
TaxiTable_train3 <- TaxiTrain[,c(2,3,5,6,28,29,31)]
TaxiTable_test3 <- TaxiTest[,c(2,3,5,6,28,29,31)]

#Table 4 = Weekday, Month, Time Interval, Pickup Geohash,pickup level prev week, temp_level, precip_level
TaxiTable_train4 <- TaxiTrain[,c(2,3,5,6,28,29,31,30)]
TaxiTable_test4 <- TaxiTest[,c(2,3,5,6,28,29,31,30)]

#Table 5 = Weekday, Month, Time Interval, Pickup Geohash,pickup level prev week, temp_level, precip_level, windspeed_level
TaxiTable_train5 <- TaxiTrain[,c(2,3,5,6,28,29,31,30,32)]
TaxiTable_test5 <- TaxiTest[,c(2,3,5,6,28,29,31,30,32)]

#Table 6 = Weekday, Month, Time Interval, Pickup Geohash, pickup level prev week, temp level, precip level, windspeed level, conditions
TaxiTable_train6 <- TaxiTrain[,c(2,3,5,6,28,29,31,30,32,27)]
TaxiTable_test6 <- TaxiTest[,c(2,3,5,6,28,29,31,30,32,27)]

#Table 7 = Weekday, Month, Time Interval, Pickup Geohash, temp, precip, windspeed level, conditions
TaxiTable_train7 <- TaxiTrain[,c(2,3,5,6,28,31,30,32,27)]
TaxiTable_test7 <- TaxiTest[,c(2,3,5,6,28,31,30,32,27)]

#Table 8 = Weekday, Month, Time Interval, Pickup Geohash, pickup level prev week, temp level, precip level, windspeed level, conditions, humidity level
TaxiTable_train8 <- TaxiTrain[,c(2,3,5,6,28,29,31,30,32,27,33)]
TaxiTable_test8 <- TaxiTest[,c(2,3,5,6,28,29,31,30,32,27,33)]


#train the models
taxi_model_1 <- naiveBayes(pickup_level~., data = TaxiTable_train1)
taxi_model_2 <- naiveBayes(pickup_level~., data = TaxiTable_train2)
taxi_model_3 <- naiveBayes(pickup_level~., data = TaxiTable_train3)
taxi_model_4 <- naiveBayes(pickup_level~., data = TaxiTable_train4)
taxi_model_5 <- naiveBayes(pickup_level~., data = TaxiTable_train5)
taxi_model_6 <- naiveBayes(pickup_level~., data = TaxiTable_train6)
taxi_model_7 <- naiveBayes(pickup_level~., data = TaxiTable_train7)
taxi_model_8 <- naiveBayes(pickup_level~., data = TaxiTable_train8)

#Test the models for overfitting
taxi_predict_train1 <- predict(taxi_model_1,TaxiTable_train1[,-5])
taxi_predict_train2 <- predict(taxi_model_2,TaxiTable_train2[,-5])
taxi_predict_train3 <- predict(taxi_model_3,TaxiTable_train3[,-5])
taxi_predict_train4 <- predict(taxi_model_4,TaxiTable_train4[,-5])
taxi_predict_train5 <- predict(taxi_model_5,TaxiTable_train5[,-5])
taxi_predict_train6 <- predict(taxi_model_6,TaxiTable_train6[,-5])
taxi_predict_train7 <- predict(taxi_model_7,TaxiTable_train7[,-5])
taxi_predict_train8 <- predict(taxi_model_8,TaxiTable_train8[,-5])

#Confusion matrix - training set
table(pred=taxi_predict_train1,true=TaxiTable_train1$pickup_level)
table(pred=taxi_predict_train2,true=TaxiTable_train2$pickup_level)
table(pred=taxi_predict_train3,true=TaxiTable_train3$pickup_level)
table(pred=taxi_predict_train4,true=TaxiTable_train4$pickup_level)
table(pred=taxi_predict_train5,true=TaxiTable_train5$pickup_level)
table(pred=taxi_predict_train6,true=TaxiTable_train6$pickup_level)
table(pred=taxi_predict_train7,true=TaxiTable_train7$pickup_level)
table(pred=taxi_predict_train8,true=TaxiTable_train8$pickup_level)

#Fraction of correct predictions - training set
mean(taxi_predict_train1==TaxiTable_train1$pickup_level)
mean(taxi_predict_train2==TaxiTable_train2$pickup_level)
mean(taxi_predict_train3==TaxiTable_train3$pickup_level)
mean(taxi_predict_train4==TaxiTable_train4$pickup_level)
mean(taxi_predict_train5==TaxiTable_train5$pickup_level)
mean(taxi_predict_train6==TaxiTable_train6$pickup_level)
mean(taxi_predict_train7==TaxiTable_train7$pickup_level)
mean(taxi_predict_train8==TaxiTable_train8$pickup_level)

#Predict test set
taxi_predict_test1 <- predict(taxi_model_1,TaxiTable_test1[,-5])
taxi_predict_test2 <- predict(taxi_model_2,TaxiTable_test2[,-5])
taxi_predict_test3 <- predict(taxi_model_3,TaxiTable_test3[,-5])
taxi_predict_test4 <- predict(taxi_model_4,TaxiTable_test4[,-5])
taxi_predict_test5 <- predict(taxi_model_5,TaxiTable_test5[,-5])
taxi_predict_test6 <- predict(taxi_model_6,TaxiTable_test6[,-5])
taxi_predict_test7 <- predict(taxi_model_7,TaxiTable_test7[,-5])
taxi_predict_test8 <- predict(taxi_model_8,TaxiTable_test8[,-5])

#confusion matrix - test set
table(pred=taxi_predict_test1,true=TaxiTable_test1$pickup_level)
table(pred=taxi_predict_test2,true=TaxiTable_test2$pickup_level)
table(pred=taxi_predict_test3,true=TaxiTable_test3$pickup_level)
table(pred=taxi_predict_test4,true=TaxiTable_test4$pickup_level)
table(pred=taxi_predict_test5,true=TaxiTable_test5$pickup_level)
table(pred=taxi_predict_test6,true=TaxiTable_test6$pickup_level)
table(pred=taxi_predict_test7,true=TaxiTable_test7$pickup_level)
table(pred=taxi_predict_test8,true=TaxiTable_test8$pickup_level)


#Fraction of correct predictions - test set
mean(taxi_predict_test1==TaxiTable_test1$pickup_level)
mean(taxi_predict_test2==TaxiTable_test2$pickup_level)
mean(taxi_predict_test3==TaxiTable_test3$pickup_level)
mean(taxi_predict_test4==TaxiTable_test4$pickup_level)
mean(taxi_predict_test5==TaxiTable_test5$pickup_level)
mean(taxi_predict_test6==TaxiTable_test6$pickup_level)
mean(taxi_predict_test7==TaxiTable_test7$pickup_level)
mean(taxi_predict_test8==TaxiTable_test8$pickup_level)

