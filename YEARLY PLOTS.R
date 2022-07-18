# yearly plots

for (i in seq(2010,2018)) {
  year = as.character(i)
  datespan = c(paste(year,"-01-01", sep=''), paste(year,"-12-31", sep=''))
  #print(datespan)
  plot(myseries$value~myseries$Data, 
       xlim=as.Date(datespan), 
       xlab = "Time", ylab = "Price Agg", type="l", main=datespan)
  
}
# yearly plots for Dy
for (i in seq(2010,2018)) {
  year = as.character(i)
  datespan = c(paste(year,"-01-01", sep=''), paste(year,"-12-31", sep=''))
  #print(datespan)
  Dyplot <- diff(myseries$value[myseries$Data <= datespan[2] & myseries$Data >= datespan[1]], lag=7, differences = 1)
  plot(Dyplot,
       xlab = "Time", ylab = "Price Agg", type="l", main=datespan)
  
}



# yearly series ACF
a<-1
b<-365
bis<-0
yearlist <- list()
for (i in seq(2010,2018)) {
  if ((i == 2012)|(i == 2016)){
    b=b+1
    #print(c(a,b,i))
    yearlyAcf <- Acf(myseries$value[a:b], lag.max = 31)
  } else {
    yearlyAcf <- Acf(myseries$value[a:b], lag.max = 31)
    #print(c(a,b,i))
    }
  yearlist <- append(yearlist, yearlyAcf)
  #print(c(a,b,i))
  a=b+1
  b=b+365
}

# yearly series PACF
a<-1
b<-365
bis<-0
yearlistPacf <- list()
for (i in seq(2010,2018)) {
  if ((i == 2012)|(i == 2016)){
    b=b+1
    #print(c(a,b,i))
    yearlyPacf <- Pacf(myseries$value[a:b], lag.max = 31)
  } else {
    yearlyPacf <- Pacf(myseries$value[a:b], lag.max = 31)
    #print(c(a,b,i))
  }
  yearlistPacf <- append(yearlistPacf, yearlyPacf)
  #print(c(a,b,i))
  a=b+1
  b=b+365
}

# yearly Df Acf
a<-1
b<-365
bis<-0
yearlist <- list()
for (i in seq(2010,2018)) {
  if ((i == 2012)|(i == 2016)){
    b=b+1
    #print(c(a,b,i))
    yearlyAcf <- Acf(diff(myseries$value[a:b], lag=7), lag.max = 31)
  } else {
    yearlyAcf <- Acf(diff(myseries$value[a:b], lag=7), lag.max = 31)
    #print(c(a,b,i))
  }
  yearlist <- append(yearlist, yearlyAcf)
  #print(c(a,b,i))
  a=b+1
  b=b+365
}

# Stationarity by year
for (i in seq(2010,2018)) {
  year = as.character(i)
  datelim = paste(year,"-12-31", sep='')
  a<-mean(myseries$value[myseries$Data < datelim])
  b<-sd(myseries$value[myseries$Data < datelim])
  alog<-mean(log(myseries$value[myseries$Data < datelim]))
  blog<-sd(log(myseries$value[myseries$Data < datelim]))
  print("Mean and SD on series:")
  print(c(a,b))
  print("Mean and SD on log-series:")
  print(c(alog,blog))
}
