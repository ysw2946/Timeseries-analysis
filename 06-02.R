library(tidyverse)
library(fpp2)
library(forecast)

Quotes <- insurance[,1]
Ad <- insurance[,2]

ndiffs(Quotes)
ndiffs(Ad)


Ad_1 <- stats::lag(Ad, -1)
Ad_2 <- stats::lag(Ad, -2)
Ad_3 <- stats::lag(Ad, -3)

cbind(1:5, lag(1:5,1))

start(Ad_1)
start(Ad_2)
start(Ad_3)
start(Quotes)
start(Ad)

d1 <- ts.intersect(Quotes, Ad, Ad_1,Ad_2,Ad_3)
d1

fit1 <- auto.arima(d1[,1], xreg=d1[,2], stationary = T)
fit2 <- auto.arima(d1[,1], xreg=d1[,2:3], stationary = T)
fit3 <- auto.arima(d1[,1], xreg=d1[,2:4], stationary = T)
fit4 <- auto.arima(d1[,1], xreg=d1[,2:5], stationary = T)

c(fit1$aicc,fit2$aicc,fit3$aicc,fit4$aicc)

d2 <- ts.intersect(Quotes, Ad, Ad_1)

fit <- auto.arima(d2[,1], xreg=d2[,2:3], stationary=T)

checkresiduals(fit)

new_Ad <- cbind(Ad = rep(mean(Ad),12),
                Ad_1 = c(dplyr::last(Ad),
                         rep(mean(Ad),11)))

fc <- forecast(fit, xreg=new_Ad)
fc

autoplot(fc)


