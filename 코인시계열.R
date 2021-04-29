library(tidyverse)
library(forecast)
library(readxl)

# xrp <- read_xlsx("c:/Data/리플2.xlsx")
xrp <- read.csv("C:/Data/리플.csv", fileEncoding="CP949")
xrp <- xrp[1:2]
#xrp <- xrp[119:1,]
xrp <- xrp[366:1,] 
#xrp.ts <- ts(xrp[2],start=c(2019,04,15),end=c(2021,04,15),freq=365)
xrp.ts <- ts(xrp[2], start=c(2020,107),freq=365)
autoplot(xrp.ts)

train <- window(xrp.ts,end=c(2021,97))

test <- window(xrp.ts,start=c(2021,98))
                
fit <- ses(train,h=10)


fit1 <- auto.arima(train,d=0, stepwise=FALSE,
                   approximation=FALSE, seasonal=FALSE)

autoplot(fit1)


fit <- ets(train,lambda=0)
summary(fit)
checkresiduals(fit)
autoplot(fit)


fc <- forecast(fit,10)
accuracy(fc,test)

autoplot(test,series="t",size=1) +
  autolayer(fc,size=1,series="p",PI=0)

autoplot(test, series="t",size=1) +
  autolayer(fc, series="p", size=1,PI=0) +
  labs(y=NULL,color=NULL)


autoplot(train) +
  autolayer(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1,PI=FALSE) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)
