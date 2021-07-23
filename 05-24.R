library(tidyverse)
library(fpp2)
library(forecast)
library(patchwork)

global <- read.table("C:/Data/global.txt",stringsAsFactors = F)
global <- as.vector(t(global))
global.ts <- ts(global, start=c(1856,1),freq=12)
autoplot(global.ts)

temp <- window(global.ts, start=1970)
train_g <- window(temp, end=c(2003,12))
test_g <- window(temp, start=c(2004,1))


TIME <- time(train_g)
MONTH <- seasonaldummy(train_g)
fit1 <- tslm(train_g ~ TIME + MONTH)
summary(fit1)

checkresiduals(fit1)

fit2 <- auto.arima(train_g,xreg=cbind(TIME,MONTH))
summary(fit2)

checkresiduals(fit2)

Time <- time(train_g)
res <- vector("numeric",6)
for(i in seq(res)){
  xreg <- cbind(Time, fourier(train_g, K=i))
  fit <- auto.arima(train_g,xreg=xreg)
  res[i] <- fit$aicc
}

res

(k_min <- which.min(res))

Time <- time(train_g)
Fourier <- fourier(train_g,K=k_min)
fit3 <- auto.arima(train_g, xreg=cbind(Time,Fourier))

checkresiduals(fit3)

fit2$aicc
fit3$aicc

fit_arima <- auto.arima(train_g)
fit_arima

checkresiduals(fit_arima)

fit_ets <- ets(train_g)
new_g3 <- cbind(Time = time(test_g),
                Fourier=fourier(test_g, K=k_min))
fc_g3 <- forecast(fit3, xreg=new_g3)
fc_ets <- forecast(fit_ets, h=length(test_g))
fc_arima <- forecast(fit_arima, h=length(test_g))

p1 <- autoplot(fc_g3, include=24) +
  autolayer(test_g, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) + ylim(-.05,1.1)

p2 <- autoplot(fc_arima, include=24) +
  autolayer(test_g, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) + ylim(-.05,1.1)

p3 <- autoplot(fc_ets, include=24) +
  autolayer(test_g, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) + ylim(-.05,1.1)

Rmisc::multiplot(p1,p2,p3)
# 여러개 그래프 한번에 그리기
# library(patchwork)
p1/p2/p3
p1+p2+p3

train_AP <- window(AirPassengers, end=c(1958,12))
test_AP <- window(AirPassengers, start=c(1959,1))

# ets
fit_ets <- ets(train_AP,lambda=0)
autoplot(fit_ets)
checkresiduals(fit_ets)

# regression
Time <- time(train_AP)
Month <- seasonaldummy(train_AP)
fit_r1 <- auto.arima(train_AP, xreg = cbind(Time,Month),
                     lambda=0)
summary(fit_r1)
checkresiduals(fit_r1)

Time <- time(train_AP)
res <- vector("numeric",6)
for(i in seq(res)){
  xreg <- cbind(Time, fourier(train_AP,K=i))
  fit <- auto.arima(train_AP, xreg=xreg,lambda=0)
  res[i] <- fit$aicc
}
(min_k <- which.min(res))

Time <- time(train_AP)
Fourier <- fourier(train_AP, K=min_k)
fit_r2 <- auto.arima(train_AP, xreg=cbind(Time, Fourier),
                     lambda=0)
summary(fit_r2)
checkresiduals(fit_r2)

fit_r1$aicc
fit_r2$aicc

ndiffs(log(train_AP))
nsdiffs(log(train_AP))

train_APd <- diff(log(train_AP),lag=12)
train_APdd <- diff(train_APd)
ggtsdisplay(train_APdd)

fit_a <- auto.arima(train_AP, lambda=0,
                    stepwise=FALSE,
                    approximation=FALSE)
checkresiduals(fit_a)

new_t <- cbind(Time = time(test_AP),
               Fourier=fourier(test_AP, K=min_k))
fc_reg <- forecast(fit_r2, xreg=new_t)
fc_ets <- forecast(fit_ets, h=length(test_AP))
fc_a <- forecast(fit_a, h=length(test_AP))

accuracy(fc_reg)
accuracy(fc_ets)
accuracy(fc_a)
