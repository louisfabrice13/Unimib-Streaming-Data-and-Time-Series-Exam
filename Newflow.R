library(forecast)
library(urca)
library(KFAS)
library(ggplot2)
library(xts)
library(timeSeries)
library(tseries)

time_series_dataset <- read.csv("C:/Users/louis/Documenti/Data_Science/2.1/Streaming DM & Time Series/Progetto/time_series_dataset.csv", sep=";")
myseries <- time_series_dataset
myseries$Data <- as.Date( myseries$Data, format="%Y-%m-%d")
plot(myseries$value~myseries$Data, xlim=as.Date(c("2010-01-01", "2018-12-31")), xlab = "Time", ylab = "Price Agg", type="l")

y <- ts(myseries$value)
lny <- log(y)
plot(lny~myseries$Data, xlim=as.Date(c("2010-01-01", "2018-12-31")), xlab = "Time", ylab = "Ln of Price Agg", type="l")
# there's no particular evidence for variance proportionality to mean

######### YEARLY CONSIDERATIONS ##############
#mean(myseries$value[myseries$Data < "2018-12-31"])
#sd(myseries$value[myseries$Data < "2018-12-31"])

#mean(log(myseries$value[myseries$Data < "2018-12-31"]))
#sd(log(myseries$value[myseries$Data < "2018-12-31"]))
######### YEARLY CONSIDERATIONS ##############
adf.test(y)
adf.test(lny)

# Let's estimate the ACF and PACF
acf_y <- Acf(y, lag.max = 31)
pacf_y <- Pacf(y, lag.max = 31)

acf_long <- Acf(y, lag.max = 370)
pacf_long <- Pacf(y, lag.max = 370)
# the PACF exponentially decaying spikes at lag-7(and multiples) + the linear
# decaying in the equivalent spikes for the ACF suggest a sARIMA(1,1,1)[7]

Dy <- diff(y, lag = 7, differences = 1)
plot(Dy, xlab = "Time", ylab = "First Seasonal Difference (weekly)", type="l")
# the plot looks now centered around zero, stationary in mean (mu=0)?
adf.test(Dy)
#DF test already confirmed stationarity, now the test statistics is just greater

acf_Dy <- Acf(Dy)
pacf_Dy <- Pacf(Dy)
# the series mantaines a strong effect of the 7-based lags
# PACF suggests AR(3) and ACF a possible MA(4)

# Splitting the data in first 8 years of training and last year of test sets
traindataset <- subset(myseries, myseries$Data <= "2017-12-31")
testdataset <- subset(myseries, myseries$Data > "2017-12-31")
train <- traindataset$value
test <- testdataset$value
plot(train, type="l", main="Train set years")
plot(test, type="l", main="Test set year")

# preparation of greed search around sARIMA(p,d,q)(P,D,Q)[7] with p in [0-3], d in [0,1], q in [0,4]
# P=1, D=1, Q=1 for now
######### CYCLE FOR GREED SEARCHES ###########
bestARIMA = c()
for (p in c(0,1,2,3)){
  #p loop
  for(q in c(0,1,2,3,4)){
    #q loop
    for(d in c(0,1)){
      #d loop
      mod <- Arima(train, c(p,d,q), seasonal=list(order=c(1,1,1), period=7))
      #summary(mod)
      if (p+d+q == 0){
        bestAICc = mod$aicc
      }# first model is the best at the beginning
      if (mod$aicc < bestAICc){
        bestAICc = mod$aicc
        bestARIMA = c(p,d,q)
        bestmod = mod
        print('new best')
      }#any betterment saved
      print(c(p,d,q))
    }
  }
  
  
}
print('done')
##############################################
#best model is actually sARIMA(3,0,4)(1,1,1)[7], (3,1,4) brings computational problems
#longer memory models have less relevant AR coefficients of higher orders, require much more
#computation and bring little to no improvement in AICc, RMSE and MAPE
#we will try only a best (by AICc) long model sARIMA(6,0,8)(1,1,1)[7]
#bestmod <- Arima(train, c(3,0,4), seasonal=list(order=c(1,1,1), period=7))


predbest <- forecast(bestmod, h=365)
autoplot(predbest)
checkresiduals(bestmod, lag = 62)
# there're still significant spikes but they hardly belong to an obvious ARMA component 
# nonetheless the residuals are definitely autocorrelated

predbestval <- predbest$mean
g <- ggplot(testdataset, aes(testdataset$Data))
g <- g + geom_line(aes(y=testdataset$value), colour="black")
g <- g + geom_line(aes(y=predbestval), colour="red")
g
# the model captures the weekly spikes, but the yearly series seems non-stationary on the mean
# much variance unexplained, regressors should be tried

#longmod <- Arima(train, c(6,0,8), seasonal=list(order=c(1,1,1), period=7))
predlong <- forecast(longmod, h=365)
autoplot(predlong)
checkresiduals(predlong, lag = 62)
# slightly better?

predlongval <- predlong$mean
k <- ggplot(testdataset, aes(testdataset$Data))
k <- k + geom_line(aes(y=testdataset$value), colour="black")
k <- k + geom_line(aes(y=predlongval), colour="red")
k
# the autocorrelation is most plausible
# see Ljung Box test
# anyway yearly plots show that different years have different local trends and seasonalities
# winter prices seem higher, but this seasonality is varying, solar vs legal hr might have a role etc
# the weekly seasonality is the only true commonality

accuracy(predbestval, test)
accuracy(predlongval, test)
# by any critical error measure, the long memory model is better, hence it would be re-estimated
# in order to generate the definitive forecast, but we add regressors, so let's keep them both

#########INTRODUZIONE REGRESSORI#############
dfy <- ts(traindataset$value, frequency = 365)

# XARIMA with best short model
bestfit <- list(aicc=Inf)
for(i in 1:25){
  fit <- Arima(dfy, xreg=fourier(dfy, K=i), order=c(3,0,4), seasonal=list(order=c(1,1,1), period=7))
  if(fit$aicc < bestfit$aicc){
    bestfit <- fit
    print("new best fit")
  }
  
  print(i)
}

# XARIMA with best long model
bestlongfit <- list(aicc=Inf)
for(i in 1:25){
  fit <- Arima(dfy, xreg=fourier(dfy, K=i), order=c(6,0,8), seasonal=list(order=c(1,1,1), period=7))
  if(fit$aicc < bestlongfit$aicc){
    bestlongfit <- fit
    print("new best fit")
  }
  
  print(i)
}
#############################################
# bestfit <- Arima(dfy, xreg=fourier(dfy, K=14), order=c(3,0,4), seasonal=list(order=c(1,1,1), period=7))

# 14 sinusoidal regressors have the best fit within 1:25
# one could jointly adapt number of regressors and ARIMA errors but
# our approach to the ARIMA model fitted better than automatic models
summary(bestfit)
# this model only slightly reduces AICc, RMSE, MAPE...
predbestfit <- forecast(bestfit, xreg=fourier(dfy, K=14, h=365))
autoplot(predbestfit)
checkresiduals(predbestfit, lag = 62)

predxarima <- predbestfit$mean
h <- ggplot(testdataset, aes(testdataset$Data))
h <- h + geom_line(aes(y=testdataset$value), colour="black")
h <- h + geom_line(aes(y=predxarima), colour="red")
h
# yet the micro-trends and cycles are more accurately followed, it is a valuable improvement

# bestlongfit <- Arima(dfy, xreg=fourier(dfy, K=14), order=c(6,0,8), seasonal=list(order=c(1,1,1), period=7))
# again 14 regressors
summary(bestlongfit)
predbestlongfit <- forecast(bestlongfit, xreg=fourier(dfy, K=14, h=365))
autoplot(predbestlongfit)
checkresiduals(predbestlongfit, lag = 62)

predlongxarima <- predbestlongfit$mean
j <- ggplot(testdataset, aes(testdataset$Data)) + ggtitle("2018 Price (black) vs XARIMA Prediction (red)") +
  xlab("Data") + ylab("value")
j <- j + geom_line(aes(y=testdataset$value), colour="black")
j <- j + geom_line(aes(y=predlongxarima), colour="red")
j

accuracy(predxarima, test)
accuracy(predlongxarima, test)
