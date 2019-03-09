rm(list = ls())

# https://stackoverflow.com/questions/24758218/time-series-data-splitting-and-model-evaluation

library(caret)
library(ggplot2)
library(pls)

data(economics)


timeSlices <- createTimeSlices(1:nrow(economics), initialWindow = 36, horizon = 12, fixedWindow = TRUE)
# 36 periods for training, and next 12 periods for testing.

# timeSlices[[1]]$Training036
# timeSlices[[1]]$Training037
#
# timeSlices[[2]]$Testing036
# timeSlices[[2]]$Testing037

# Train the model.
trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]

plsFitTime <- train(unemploy ~ pce + pop + psavert,
                    data = economics[trainSlices[[1]],],
                    method = "pls",
                    preProc = c("center", "scale"))


pred <- predict(plsFitTime, economics[testSlices[[1]],])

true <- economics$unemploy[testSlices[[1]]]

plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue")

# Train all the models at one time.
myTimeControl <- trainControl(method = "timeslice", initialWindow = 36, horizon = 12, fixedWindow = TRUE)


plsFitTime <- train(unemploy ~ pce + pop + psavert,
                    data = economics,
                    method = "pls",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)


plsFitTime




#---------------------------------------------------------------------------------------------------------------#
# Codes by Rob J Hyndman
#---------------------------------------------------------------------------------------------------------------#
library(fpp) # To load the data set a10
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
plot(log(a10), ylab="", xlab="Year", main="Log Antidiabetic drug sales")

k <- 60 # minimum data length for fitting a model
n <- length(a10) # 204 data points, single data series.
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12) # Save mean absolute error for the test data.
st <- tsp(a10)[1]+(k-2)/12 # Starting year + (k-2)/12

i <- 1

for(i in 1:(n-k))
{
  xshort <- window(a10, end = st + i/12) # Subset the dataset for training. (60 observations)
  xnext <- window(a10, start=st + (i+1)/12, end=st + (i+12)/12) # Subset the dataset for testing (12 observations)
  fit1 <- tslm(xshort ~ trend + season, lambda=0) # Linear model with time series component.
  fcast1 <- forecast(fit1, h=12) # Forcast the next 12 periods.
  fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12),
                include.drift=TRUE, lambda=0, method="ML") # Fit ARIMA model.
  fcast2 <- forecast(fit2, h=12) # Forcast the next 12 periods.
  fit3 <- ets(xshort,model="MMM",damped=TRUE) # Exponential smoothing state space model.
  fcast3 <- forecast(fit3, h=12) # Forcast the next 12 periods.
  mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext) # absolute value of errors.
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.65,1.05))
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)


#---------------------------------------------------------------------------------------------------------------#
# Lasso regression. Reference: https://www.r-bloggers.com/ridge-regression-and-the-lasso/
#---------------------------------------------------------------------------------------------------------------#
swiss <- datasets::swiss
library(glmnet)

x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility
lambda <- 10^seq(10, -2, length = 100)

set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]


ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0, lambda = lambda)

plot(cv.out)
bestlam <- cv.out$lambda.min

# 10-fold cross validation.













