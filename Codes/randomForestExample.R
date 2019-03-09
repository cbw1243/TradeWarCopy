library(randomForest)
library(rvest)
library(readr)
library(data.table)

#-----------------------------------------------------------------------------------------------------------------#
data1 <- read.table('../Data/car.txt', sep = ',', header = FALSE)
colnames(data1) <- c('BuyingPrice', 'Maintenance', 'NumDoors', 'NumPersons', 'BootSpace', 'Safety', 'Condition')

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]


# Create a Random Forest model with default parameters
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
# By default, number of trees is 500 and number of variables tried at each split is 2 in this case
model1


model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Condition)


#-----------------------------------------------------------------------------------------------------------------#
# Parameter tuning.
# Example is from: https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/tutorial-random-forest-parameter-tuning-r/tutorial/
rm(list = ls())
library(randomForest)
library(data.table)
library(mlr)
library(h2o)

setcol <- c("age", "workclass", "fnlwgt", "education", "education-num",
            "marital-status", "occupation", "relationship", "race", "sex",
            "capital-gain", "capital-loss", "hours-per-week", "native-country", "target")

#load data
train <- read.table("../Data/adultdata.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"),stringsAsFactors = F)
test <- read.table("../Data/adulttest.txt",header = F,sep = ",",col.names = setcol,skip = 1, na.strings = c(" ?"),stringsAsFactors = F)

setDT(train)
setDT(test)

imp1 <- impute(data = train,target = "target",classes = list(integer=imputeMedian(), factor=imputeMode()))
imp2 <- impute(data = test,target = "target",classes = list(integer=imputeMedian(), factor=imputeMode()))
train <- imp1$data
test <- imp2$data











#-----------------------------------------------------------------------------------------------------------------#
# Implement the random forest in R. Source: https://www.r-bloggers.com/random-forests-in-r/
#-----------------------------------------------------------------------------------------------------------------#
require(randomForest)
require(MASS)#Package which contains the Boston housing dataset
data(Boston)
dim(Boston)

set.seed(101)
train <- sample(1:nrow(Boston), 300) # Use 300 out of 506 observations as training data.

Boston.rf <- randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf
Boston.rf$mse[400]
plot(Boston.rf)

oob.err <- double(13)
test.err <- double(13)

# mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) {
  rf=randomForest(medv ~ . , data = Boston , subset = train, mtry = mtry, ntree=400)
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted

  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error

  cat(mtry," ") #printing the output to the console
}

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))




