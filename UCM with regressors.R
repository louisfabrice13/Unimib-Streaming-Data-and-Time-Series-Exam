trainidx <- 1:2922 # Campione per la stima (fino a metà 2017)
testidx <- 2923:3287 # Campione per valutare le previsioni (2018)
tot <- 1:3287

ytrain <- myseries$value[trainidx]
vary <- var(ytrain)

freq <- outer(tot, 1:14)*2*pi/365 # 14 sinusoids of annual fundamental period
cs <- cos(freq[trainidx, ]) # cosines
colnames(cs) <- paste("cos", 1:14)
si <- sin(freq[trainidx, ]) # sines
colnames(si) <- paste("cos", 1:14)

# to complete forecast
cs_ <- cos(freq[testidx, ])
colnames(cs_) <- paste("cos", 1:14)
si_ <- sin(freq[testidx, ])
colnames(si_) <- paste("cos", 1:14)
css <- rbind(cs, cs_)
sii <- rbind(si, si_)

mod3 <- SSModel(ytrain ~ cs+si+
                  SSMtrend(2, list(NA, NA))+
                  SSMseasonal(7, NA, "dummy"),
                H = NA)

# Initialization with data extimates
cfs <- bestlongfit$coef[17:44] # fourier regressors
mod3$a1[1:28] <- cfs 
mod3$a1[29] <- y[1] # first datapoint as intercept
mod3$P1inf <- matrix(0, 36, 36) 
diag(mod3$P1[1:28, 1:28]) <- diag(bestlongfit$var.coef[17:44, 17:44])
diag(mod3$P1[29:36, 29:36]) <- log(vary) 

fit3 <- fitSSM(mod3, rep(log(vary), 4))
fit3$optim.out$convergence

# First Results
smo3 <- KFS(fit3$model, smoothing = "state")
plot(timeSeries(ytrain, as.Date("2010-01-01") + 0:(length(ytrain)-1)))
lines(timeSeries(smo3$alphahat[, "level"], as.Date("2010-01-01") + 0:(length(ytrain)-1)),
      col = "red")
one_step_ahead <- smo3$alphahat[, "level"]

# Compute RMSE on train
extimate <- one_step_ahead
actual <- ytrain
sqrt(mean((extimate - actual)^2))
# Compute MAPE on train
mean(abs(extimate - actual) / actual) * 100

# To extend to test set, let's have a prolonged empty model, filled by the filter

mod3_ <- SSModel(c(myseries$value[trainidx], rep(NA, length(testidx)))~ css + sii+
                   SSMtrend(2, Q=list(fit3$model$Q[1, 1, 1], fit3$model$Q[2, 2, 1]))+
                   SSMseasonal(7, fit3$model$Q[3, 3, 1], "dummy"),
                 H = exp(fit3$optim.out$par[4]))
mod3_$a1 <- fit3$model$a1
mod3_$P1 <- fit3$model$P1
mod3_$P1inf <- fit3$model$P1inf
smo3_ <- KFS(mod3_, smoothing = c("state", "signal"))
plot(timeSeries(myseries$value[testidx], as.Date("2018-01-01") + 0:(length(myseries$value[testidx])-1)))
lines(timeSeries(smo3_$muhat[testidx, 1],as.Date("2018-01-01") + 0:(length(myseries$value[testidx])-1)),
      col = "red")

# Compute RMSE on test
one_step_ahead3 <- smo3_$muhat[testidx, 1]
extimate <- one_step_ahead3
actual <- y[testidx]
sqrt(mean((extimate - actual)^2))

# Compute MAPE on test
mean(abs(extimate - actual) / actual) * 100

m <- ggplot(testdataset, aes(testdataset$Data))+ ggtitle("2018 Price (black) vs UCM Prediction (red)") +
  xlab("Data") + ylab("value")
m <- m + geom_line(aes(y=testdataset$value), colour="black")
m <- m + geom_line(aes(y=smo3_$muhat[testidx, 1]), colour="red")
m

#################### RETRY THE MODEL WITH SIMPLER TREND

mod4 <- SSModel(ytrain ~ cs+si+
                  SSMtrend(1, NA)+
                  SSMseasonal(7, NA, "dummy"),
                H = NA)
cfs <- bestlongfit$coef[17:44] 
mod4$a1[1:28] <- cfs
mod4$a1[29] <- y[1] 
mod4$P1inf <- matrix(0, 35, 35)
diag(mod4$P1[1:28, 1:28]) <- diag(bestlongfit$var.coef[17:44, 17:44])
diag(mod4$P1[29:35, 29:35]) <- log(vary)
fit4 <- fitSSM(mod4, rep(log(vary), 3))
fit4$optim.out$convergence

# First Results
smo4 <- KFS(fit4$model, smoothing = "state")
plot(timeSeries(ytrain, as.Date("2010-01-01") + 0:(length(ytrain)-1)))
lines(timeSeries(smo4$alphahat[, "level"], as.Date("2010-01-01") + 0:(length(ytrain)-1)),
      col = "red")
one_step_ahead <- smo4$alphahat[, "level"]

# Compute RMSE on train
extimate <- one_step_ahead
actual <- ytrain
sqrt(mean((extimate - actual)^2))
# Compute MAPE on train
mean(abs(extimate - actual) / actual) * 100

mod4_ <- SSModel(c(myseries$value[trainidx], rep(NA, length(testidx)))~ css + sii+
                   SSMtrend(1, Q=fit4$model$Q[1, 1, 1])+
                   SSMseasonal(7, fit4$model$Q[2, 2, 1], "dummy"),
                 H = exp(fit4$optim.out$par[3]))
# Poniamo anche le condizioni iniziali come nel modello precedente
mod4_$a1 <- fit4$model$a1
mod4_$P1 <- fit4$model$P1
mod4_$P1inf <- fit4$model$P1inf
# Calcoliamo lo smoothing delle variabili di stato e del "segnale"
smo4_ <- KFS(mod4_, smoothing = c("state", "signal"))
plot(timeSeries(myseries$value[testidx], as.Date("2018-01-01") + 0:(length(myseries$value[testidx])-1)))
lines(timeSeries(smo4_$muhat[testidx, 1],as.Date("2018-01-01") + 0:(length(myseries$value[testidx])-1)),
      col = "red")

# Compute RMSE on test
one_step_ahead4 <- smo4_$muhat[testidx, 1]
extimate <- one_step_ahead4
actual <- y[testidx]
sqrt(mean((extimate - actual)^2))

# Compute MAPE on test
mean(abs(extimate - actual) / actual) * 100