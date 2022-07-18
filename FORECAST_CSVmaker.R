######### PREDICTION MAKER

# DATE
Data <- seq(as.Date("2019-01-01"), by=1, length.out=334)

# XARIMA
y <- ts(myseries$value, frequency=365)
bestlongfit <- Arima(y, xreg=fourier(y, K=14), order=c(6,0,8), seasonal=list(order=c(1,1,1), period=7))
# again 14 regressors
summary(bestlongfit)
predbestlongfit <- forecast(bestlongfit, xreg=fourier(y, K=14, h=334))
autoplot(predbestlongfit)
checkresiduals(predbestlongfit, lag = 62)

predlongxarima <- predbestlongfit$mean
plot(timeSeries(predlongxarima, Data), col="blue", lwd=2,
     main="2019 XARIMA Forecast", sub="01-01 to 30-11",
     xlab='Time steps', ylab='value per XARIMA')
grid(nx=11, ny=5)

ARIMA <- predlongxarima

# UCM

tot <- 1:3287
ext <- 1:(3287+334)
forec <- 3288:(3287+334)

freq <- outer(ext, 1:14)*2*pi/365 # 14 sinusoids of annual fundamental period
cs <- cos(freq[tot, ]) # cosines
colnames(cs) <- paste("cos", 1:14)
si <- sin(freq[tot, ]) # sines
colnames(si) <- paste("cos", 1:14)

# to complete forecast
cs_ <- cos(freq[forec, ])
colnames(cs_) <- paste("cos", 1:14)
si_ <- sin(freq[forec, ])
colnames(si_) <- paste("cos", 1:14)
css <- rbind(cs, cs_)
sii <- rbind(si, si_)

yext <- ext
yext[tot] <- y
yext[forec] <- NA
mod3 <- SSModel(y ~ cs+si+
                  SSMtrend(2, list(NA, NA))+
                  SSMseasonal(7, NA, "dummy"),
                H = NA)

# Initialization with data extimates
cfs <- bestlongfit$coef[17:44] # fourier regressors
mod3$a1[1:28] <- cfs 
mod3$a1[29] <- y[1] # first datapoint as intercept
mod3$P1inf <- matrix(0, 36, 36) 
diag(mod3$P1[1:28, 1:28]) <- diag(bestlongfit$var.coef[17:44, 17:44])
vary <- var(y)
diag(mod3$P1[29:36, 29:36]) <- log(vary) 

fit3 <- fitSSM(mod3, rep(log(vary), 4))
fit3$optim.out$convergence

# First Results
smo3 <- KFS(fit3$model, smoothing = "state")
plot(timeSeries(y, as.Date("2010-01-01") + 0:(length(y)-1)))
lines(timeSeries(smo3$alphahat[, "level"], as.Date("2010-01-01") + 0:(length(y)-1)),
      col = "red")
one_step_ahead <- smo3$alphahat[, "level"]

# Compute RMSE on train
extimate <- one_step_ahead
actual <- y
sqrt(mean((extimate - actual)^2))
# Compute MAPE on train
mean(abs(extimate - actual) / actual) * 100

# To extend let's have a prolonged empty model, filled by the filter

mod3_ <- SSModel(yext~ css + sii+
                   SSMtrend(2, Q=list(fit3$model$Q[1, 1, 1], fit3$model$Q[2, 2, 1]))+
                   SSMseasonal(7, fit3$model$Q[3, 3, 1], "dummy"),
                 H = exp(fit3$optim.out$par[4]))
mod3_$a1 <- fit3$model$a1
mod3_$P1 <- fit3$model$P1
mod3_$P1inf <- fit3$model$P1inf
smo3_ <- KFS(mod3_, smoothing = c("state", "signal"))

plot(timeSeries(smo3_$muhat[forec], Data), col="red", lwd=2,
     main="2019 UCM Forecast", sub="01-01 to 30-11",
     xlab='Time steps', ylab='value per UCM')
grid(nx=11, ny=5)

UCM <- smo3_$muhat[forec]
  
# RNN
ML <- read.csv("C:/Users/louis/Documenti/Data_Science/2.1/Streaming DM & Time Series/Progetto/2019LSTMforecast.csv")


LSTMtest2018 <- read.csv("C:/Users/louis/Documenti/Data_Science/2.1/Streaming DM & Time Series/Progetto/2018LSTMforecast.csv")
accuracy(ts(LSTMtest2018$X0), test)
n <- ggplot(testdataset, aes(testdataset$Data)) + ggtitle("2018 Price (black) vs LSTM Prediction (red)") +
  xlab("Data") + ylab("value")
n <- n + geom_line(aes(y=testdataset$value), colour="black")
n <- n + geom_line(aes(y=LSTMtest2018$X0), colour="red")
n


plot(timeSeries(ML$X0, Data), col="chartreuse3", lwd=2,
     main="2019 LSTM Forecast", sub="01-01 to 30-11",
     xlab='Time steps', ylab='value per LSTM')
grid(nx=11, ny=5)

######
SDMTSA_847529_1 <- cbind(as.character(Data), as.numeric(ARIMA), as.numeric(UCM), as.numeric(ML$X0))
colnames(SDMTSA_847529_1) <- c("Data", "ARIMA", "UCM", "ML")
write.csv(SDMTSA_847529_1, "C:/Users/louis/Documenti/Data_Science/2.1/Streaming DM & Time Series/Progetto/SDMTSA_847529_1.csv")
############