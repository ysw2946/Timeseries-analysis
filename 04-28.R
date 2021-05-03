library(tidyverse)
library(forecast)
library(fpp2)

tour <- scan("C://Data/Ktour.txt")
tour.ts <- ts(tour, start=1981, freq=12)

train_K <- window(tour.ts, end=c(1990,12))
test_K <- window(tour.ts, start=c(1991,1))

lntrain_K <- log(train_K)
ggtsdisplay(train_K)
ggtsdisplay(lntrain_K)

lntrain_KD <- diff(lntrain_K, lag=12)
lntrain_KDd <- diff(lntrain_K, lag.max=36)
ggtsdisplay(lntrain_KD)
ggtsdisplay(lntrain_KDd)

fit1 <- auto.arima(train_K, lambda = 0, stepwise=FALSE,
                   approximation = FALSE)
fit1

fit2 <- ets(train_K, lambda = 0)
fit2

checkresiduals(fit1)
checkresiduals(fit2)

fc1 <- forecast(fit1)
fc2 <- forecast(fit2)

accuracy(fc1,test_K)
accuracy(fc2,test_K)

autoplot(fc1, include=0) +
  autolayer(test_K, series="test")

autoplot(fc2, include=0) +
  autolayer(test_K, series="test")

# ets_bag <- baggedModel(train_K, lambda=0)
# fc3 <- forecast(ets_bag)

# arima_bag <- baggedModel(train_K, fn=auto.arima,lambda=0)
# fc4 <- forecast(arima_bag)

