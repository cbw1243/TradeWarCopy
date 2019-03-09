rm(list = ls())

# Evaluate the trade war effect on trade using OLS, Lasso, Random forest, Gradient boosting, and XG boosting.

library(dplyr)
library(tidyr)
library(data.table)
library(countrycode)
library(ggplot2)
library(pampe)
library(glmnet)
library(randomForest)
library(stringr)
library(gbm)

#-------------------------------------------------------------------------------------------------------------#
# Prepare the data for analysis.
#-------------------------------------------------------------------------------------------------------------#
soyDat <- fread('../Data/USSoybeanMonthExportClean.csv') %>%
  mutate(Time = paste(Year, Month, sep = '-'),
         Time = as.Date(paste(Time, 1, sep = '-'), format = c('%Y-%m-%d')),
         ExportValue = ExportValue/1000) %>%
  mutate(Partner = str_replace_all(Partner, fixed("(*)"), "")) %>%
  mutate(Partner = case_when(grepl('Bahamas', Partner) ~ 'Bahamas',
                             grepl('Korea, North', Partner) ~ 'NorthKorea',
                             grepl('Korea, South', Partner) ~ 'SouthKorea',
                             grepl('New Zealand', Partner) ~ 'NewZealand',
                             grepl('Ivoire', Partner) ~ 'IvoryCoast',
                             grepl('Uzbekistan', Partner) ~ 'Uzbekistan',
                             TRUE ~ Partner)) %>%
  mutate(Partner = str_replace_all(Partner, fixed(" "), ""))

wideFormatDat <- soyDat %>%
  select(Partner, Time, ExportValue) %>%
  spread(Partner, ExportValue)

wideFormatDat[is.na(wideFormatDat)] <- 0
rownames(wideFormatDat) <- wideFormatDat$Time
wideFormatDat$Time <- NULL

which(apply(wideFormatDat[1:202, ], 2, sd) == 0)

#-------------------------------------------------------------------------------------------------------------#
# Analyze the soybean export data.
#-------------------------------------------------------------------------------------------------------------#
# 1. Data description.
# 1.1 Export to China Versus total export
soyExport <- soyDat %>%
  mutate(Group = ifelse(Partner == 'China', 'China', 'ROW')) %>%
  group_by(Year, Time, Group) %>%
  summarise(ExportValue = sum(ExportValue))

plot1 <- soyExport %>%
  ggplot(data = .) +
  geom_line(aes(Time, ExportValue, color = Group))

plot1

# Tariff on Solar Panel and washing machine in October, 2017.


#----------------------------------------------------------------------------------------------------------------------#
# Linear model evaluation, with AIC selection.
#----------------------------------------------------------------------------------------------------------------------#
# Major partners in recent 10 years.
MajorPartner <- soyDat %>%
  filter(Year >= 2009) %>%
  group_by(Partner) %>%
  summarise(ExportValue = sum(ExportValue)) %>%
  arrange(desc(ExportValue))

top20partners <- MajorPartner %>%
  slice(1:20) %>%
  select(Partner) %>%
  unlist()

soyExportEst <- soyDat %>%
  filter(Partner %in% top20partners) %>%
  select(Partner, Time, ExportValue) %>%
  spread(Partner, ExportValue)

soyExportEst[is.na(soyExportEst)] <- 0
rownames(soyExportEst) <- soyExportEst$Time
soyExportEst$Time <- NULL

# summary(soyExportEst)

cutPeriod <- 202 #Period of receiving treatment.
timePeriod <- 1:cutPeriod
testPeriod <- (cutPeriod + 1):215

reg <- pampe(time.pretr = timePeriod, time.tr = testPeriod, treated = 'China', data = soyExportEst)

# Get the fitted model.
modelFit <- reg$model

# Calculate the difference.
chinaColumn <- which(colnames(soyExportEst) == 'China')
xTreat <- soyExportEst[testPeriod, - chinaColumn]
yObs <- soyExportEst[testPeriod, chinaColumn ]
yPred <- predict(modelFit, xTreat)
sum(yPred - yObs) # 13.7 billion dollars.


# Visualize the prediction.
evalEffect <-
  data.frame(predValue = predict(modelFit, soyExportEst)/1000, # in billion dollars
                         ObsValue = soyExportEst$China/1000,
                         Time = as.Date(row.names(soyExportEst))) %>%
  gather(Variable, Value, 1:2)


g <-
  ggplot(data = evalEffect) +
  geom_line(aes(Time, Value, group = Variable, color = Variable)) +
  theme_classic() +
  scale_color_manual(values = c('#d7191c', '#1a9641'),
                     labels = c('Observed', 'Predicted')) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  theme(legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = '', y = 'Export values in billion dollars',
       title = 'Figure. Trade War Impacts on Soybean Exports to China.')

g

ggsave(g, filename = '../FiguresTables/Fig_SoybeanExt.png', height = 6, width = 7, units = 'in')



# Omitted variable bias, direction.
# library('MASS')
# xValues = mvrnorm(n = 1000, mu=c(0, 0), Sigma=matrix(c(1, -0.8, -0.8, 1), nrow=2), empirical=TRUE)
# cor(xValues)
#
# y <- -2*xValues[, 1] + 3*xValues[, 2] + rnorm(1000, 0, 3)
#
# regDat <- data.frame(xValues) %>%
#   mutate(Y = y)
#
# reg <- lm(Y ~ X1 + X2, data = regDat)
# summary(reg)
#
# reg2 <- lm(Y ~ X1, data = regDat)
# summary(reg2)





#----------------------------------------------------------------------------------------------------------------------#
# Lasso evaluation, with time series cross validation for lambda selection.
#----------------------------------------------------------------------------------------------------------------------#
chinaColumn <- which(colnames(wideFormatDat) == 'China')
cutPeriod <- 202 #Period of receiving treatment.
timePeriod <- 1:cutPeriod
testPeriod <- (cutPeriod + 1):215
x <- as.matrix(wideFormatDat[timePeriod, -chinaColumn])
y <- wideFormatDat[timePeriod, chinaColumn]

# Time series cross validation for choosing lambda.
minSeries <- 100

crossValid <- function(i, lambdaChoice){
xTrain <- as.matrix(wideFormatDat[1:(minSeries + i), -chinaColumn])
yTrain <- wideFormatDat[1:(minSeries + i), chinaColumn]

lasso.train <- glmnet(xTrain, yTrain, alpha = 1, lambda = lambdaChoice) # Lasso when alpha = 1

xTest <- as.matrix(wideFormatDat[(minSeries + i + 1): cutPeriod, -chinaColumn])
yTest <- wideFormatDat[(minSeries + i + 1): cutPeriod, chinaColumn]

yPredict <- predict(object = lasso.train, newx = xTest)
MeanSquareError <- mean((yTest - yPredict)^2) # Calculate mean squared error.
MeanSquareError
}

iChoice <- 1:(cutPeriod - minSeries)
lambdaChoice <- seq(1, 50, by = 0.5) # The optimal lambda seems to be around 13000.

timeSeriesMSE <- mapply(crossValid, i = rep(iChoice, length(lambdaChoice)),
                        lambdaChoice = rep(lambdaChoice, each = length(iChoice)), SIMPLIFY = T)

MSEOut <-
  data.frame(iChoice = rep(iChoice, length(lambdaChoice)),
                     lambdaChoice = rep(lambdaChoice, each = length(iChoice)),
                     MSE = timeSeriesMSE) %>%
  group_by(lambdaChoice) %>%
  summarise(MSE = mean(MSE)) # Take the mean across all the validation series.

# (cutPeriod - minSeries)
ggplot(data = MSEOut) +
  geom_line(aes(lambdaChoice, MSE))

# Optimal lambda = 15.5
lasso.mod <- glmnet(x, y, alpha = 1, lambda = 15.5) # Lasso when alpha = 1

xPredict <- as.matrix(wideFormatDat[testPeriod, -chinaColumn])
yObs <- wideFormatDat[testPeriod, chinaColumn]

yPredict <- predict(object = lasso.mod, xPredict)

plot(yPredict, type = 'l', ylim = c(0, 3000), lty = 2)
lines(yObs, col = 'red')

# Total difference:
sum(yPredict - yObs) # 16 billion dollars from November 2017 to November 2018.


#----------------------------------------------------------------------------------------------------------------------#
# Ridge evaluation, with time series cross validation for lambda selection. (similar to Lasso evaluation)
#----------------------------------------------------------------------------------------------------------------------#
# Exclude the small importers.
totalImport <- apply(wideFormatDat, 2, sum)
totalImportShare <- totalImport/sum(totalImport)
ActiveImporters <- which(totalImportShare >= 0.001)
wideFormatDatSelect <- wideFormatDat[, ActiveImporters]


chinaColumn <- which(colnames(wideFormatDatSelect) == 'China')
cutPeriod <- 202 #Period of receiving treatment.
timePeriod <- 1:cutPeriod
testPeriod <- (cutPeriod + 1):215
x <- as.matrix(wideFormatDatSelect[timePeriod, -chinaColumn])
y <- wideFormatDatSelect[timePeriod, chinaColumn]

# Time series cross validation for choosing lambda.
minSeries <- 100

crossValid <- function(i, lambdaChoice){
  xTrain <- as.matrix(wideFormatDatSelect[1:(minSeries + i), -chinaColumn])
  yTrain <- wideFormatDatSelect[1:(minSeries + i), chinaColumn]

  lasso.train <- glmnet(xTrain, yTrain, alpha = 0, lambda = lambdaChoice) # Ridget when alpha = 0.

  xTest <- as.matrix(wideFormatDatSelect[(minSeries + i + 1): cutPeriod, -chinaColumn])
  yTest <- wideFormatDatSelect[(minSeries + i + 1): cutPeriod, chinaColumn]

  yPredict <- predict(object = lasso.train, newx = xTest)
  MeanSquareError <- mean((yTest - yPredict)^2) # Calculate mean squared error.
  MeanSquareError
}

iChoice <- 1:(cutPeriod - minSeries)
lambdaChoice <- seq(310, 320, by = 0.5) # The optimal lambda seems to be around 13000.

timeSeriesMSE <- mapply(crossValid, i = rep(iChoice, length(lambdaChoice)),
                        lambdaChoice = rep(lambdaChoice, each = length(iChoice)), SIMPLIFY = T)

MSEOut <-
  data.frame(iChoice = rep(iChoice, length(lambdaChoice)),
             lambdaChoice = rep(lambdaChoice, each = length(iChoice)),
             MSE = timeSeriesMSE) %>%
  group_by(lambdaChoice) %>%
  summarise(MSE = mean(MSE)) # Take the mean across all the validation series.

# (cutPeriod - minSeries)
ggplot(data = MSEOut) +
  geom_line(aes(lambdaChoice, MSE))

# Optimal lambda = 1490
ridget.mod <- glmnet(x, y, alpha = 0, lambda = 314.5) # Ridget when alpha = 0.

xPredict <- as.matrix(wideFormatDatSelect[testPeriod, -chinaColumn])
yObs <- wideFormatDatSelect[testPeriod, chinaColumn]

yPredict <- predict(object = ridget.mod, xPredict)

plot(yPredict, type = 'l', ylim = c(0, 5000), lty = 2)
lines(yObs, col = 'red')

# Total difference:
sum(yPredict - yObs) # 144.6 billion dollars from November 2017 to November 2018.
# Not working very well.


#----------------------------------------------------------------------------------------------------------------------#
# Random forest evaluation, focus on the hyperparameter mtry.
#----------------------------------------------------------------------------------------------------------------------#
chinaColumn <- which(colnames(wideFormatDat) == 'China')
cutPeriod <- 202 #Period of receiving treatment.
trainPeriod <- 1:cutPeriod
testPeriod <- (cutPeriod + 1):215
yObs <- wideFormatDat[testPeriod, chinaColumn]

# Arbitrary choice of the mtry parameter, and show the prediction.
RFModel <- randomForest(China ~., data = wideFormatDat[trainPeriod, ], mtry = 20)

yPredict <- predict(object = RFModel, wideFormatDat[testPeriod, ])
plot(yPredict, type = 'l', ylim = c(0, 5000), lty = 2)
lines(yObs, col = 'red')

# Total difference:
sum(yPredict - yObs) # 11.37 billion dollars from November 2017 to November 2018.


# Time series cross validation for random forest models.
minSeries <- 100

crossValidRMFunc <- function(i, mtryChoice){
  trainPeriodCV <- 1:(minSeries + i)
  testPeriodCV <- (minSeries + i + 1):cutPeriod
  yObsCV <- wideFormatDat[testPeriodCV, 'China']

  RFModelCV <- randomForest(China ~., data = wideFormatDat[trainPeriod, ], mtry = mtryChoice)

  yPredictCV <- predict(object = RFModelCV, wideFormatDat[testPeriodCV, ])
  MeanSquareError <- mean((yPredictCV - yObsCV)^2) # Calculate mean squared error.
  MeanSquareError
}

# Calculate the average MSE across mtry from 10 to 200 (by 10)
mtryMSE <- data.frame(mtry = seq(10, 200, 10), aveMSE = NA)

# Warning: the cross validation takes time (at least in my Windows computer.)
for(j in 1:nrow(mtryMSE)){
mtryChoiceJ <- mtryMSE$mtry[j]
iChoice <- 1:(cutPeriod - minSeries)
aveMSE <- mean(sapply(iChoice, function(i) crossValidRMFunc(i, mtryChoiceJ)))
mtryMSE$aveMSE[j] <- aveMSE
}

system.time()

RFModel <- randomForest(China ~., data = wideFormatDat[trainPeriod, ], mtry = 80)

yPredict <- predict(object = RFModel, wideFormatDat[testPeriod, ])
plot(yPredict, type = 'l', ylim = c(0, 5000), lty = 2)
lines(yObs, col = 'red')

# Total difference:
sum(yPredict - yObs) # 11.7 billion dollars from November 2017 to November 2018.


#----------------------------------------------------------------------------------------------------------------------#
# Gradient boosting evaluation, tuning three parameters: (1) number of trees; (2) shrinkage parameter;
#                                                        (3) number of split in each tree
#----------------------------------------------------------------------------------------------------------------------#
# Taking logs of the trade data.
wideFormatDatLog <- as.matrix(wideFormatDat)
wideFormatDatLog[which(wideFormatDatLog == 0)] <- 1
wideFormatDatLog <- log(wideFormatDatLog)
wideFormatDatLog <- as.data.frame(wideFormatDatLog)

cutPeriod <- 202 #Period of receiving treatment.
trainPeriod <- 1:cutPeriod
testPeriod <- (cutPeriod + 1):215
yObs <- wideFormatDatLog[testPeriod, chinaColumn]

# Arbitrary choice of the hyperparameter, and show the prediction. Hyperparameters not yet tuned.
boostModel <- gbm(China~., data = wideFormatDatLog[trainPeriod, -c(33, 82)], distribution= "gaussian",
                  n.trees = 500, shrinkage = 0.01, interaction.depth = 4, train.fraction = 0.8)
boostModel

yPredict = predict(boostModel, newdata = wideFormatDat[testPeriod, ], n.trees = 5000)
# The number of trees can be tuned based on n.trees in the predict function.

# Tuning the parameter on number of trees.
# Time series cross validation for random forest models.
minSeries <- 100

# Fit a model with certain number of trees (maximum)
i <- 1
crossValidRMFunc <- function(i, ShrinkagePara, InteractPara, BagFractionPara){
  trainPeriodCV <- 1:(minSeries + i)
  testPeriodCV <- (minSeries + i + 1):cutPeriod
  yObsCV <- wideFormatDatLog[testPeriodCV, 'China']

  boostModelCV <- gbm(China~., data = wideFormatDatLog[trainPeriodCV, ], distribution= "gaussian",
                    n.trees = 5000, shrinkage = ShrinkagePara, interaction.depth = InteractPara,
                    bag.fraction = BagFractionPara, cv.folds = 5)
  # Choose for the optimal number of trees for calculating the mean squared error.
  optimal_trees <- which.min(boostModelCV$cv.error)
  yPredictCV <- predict(object = boostModelCV, wideFormatDat[testPeriodCV, ], n.trees = optimal_trees)
  MeanSquareError <- mean((yPredictCV - yObsCV)^2) # Calculate mean squared error.
  MeanSquareError
}


# Grid search for searching the optimal hyperparameters that return the lowest MSE.
# Reference: http://uc-r.github.io/gbm_regression
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(0.001, .01, .1),
  interaction.depth = c(1, 3, 4),
  bag.fraction = c(.65, .8, 1),    # bag.fraction < 1 triggers stochastic gradient boosting.
  min_MSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 27

for (j in 1:nrow(hyper_grid)){
 hyper_grid$min_MSE[j] <- mean(sapply(1:3, #(cutPeriod - minSeries),
                                   function(k) crossValidRMFunc(k, ShrinkagePara = hyper_grid$shrinkage[j],
                                              InteractPara = hyper_grid$interaction.depth[j],
                                              BagFractionPara = hyper_grid$bag.fraction[j])))
 cat(hyper_grid$min_MSE[j], ';')
}

sum(yPredict - yObs) # 10.7 billion dollars


#----------------------------------------------------------------------------------------------------------------------#
# XGBoosting evaluation, tuning three parameters: (1) number of trees; (2) shrinkage parameter;
#                                                        (3) number of split in each tree
#----------------------------------------------------------------------------------------------------------------------#
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
