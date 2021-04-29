library(tidyverse)
library(forecast)
library(readxl)
library(fpp2)

a <- read.csv("C:/Data/인구동태.csv")
a <- t(a)


a <- as.data.frame(a)
colnames(a) <- a[1,]
a <- a[-1,]
a$`출생아수(명)` <- as.integer(a$`출생아수(명)`)
a$`사망자수(명)` <- as.integer(a$`사망자수(명)`)
a$`혼인건수(건)` <- as.integer(a$`혼인건수(건)`)
a$`이혼건수(건)` <- as.integer(a$`이혼건수(건)`)
str(a)


a.ts <- ts(a$`이혼건수(건)`,start=1970, freq=1)

a.ts

autoplot(a.ts)


train <- window(a.ts , end=2018)
test <- window(a.ts, start=2019)

fit <- ets(train)

autoplot(fit)
checkresiduals(fit)

fc <- forecast(fit, h = length(test))
accuracy(fc, test)

autoplot(fc) +
  autolayer(test, series="Test data", size=1) +
  labs(y= NULL, color=NULL)

autoplot(fc, include = 0) +
  autolayer(test, series="Test data", size=1) +
  labs(y= NULL, color=NULL)





train_air <- window(ausair, end=2011)
test_air <- window(ausair,start=2012)

fit1 <- ets(train_air)
autoplot(fit1)
checkresiduals(fit1)

fc1 <- forecast(fit1, h = length(test_air))
accuracy(fc1, test_air)

autoplot(fc1) +
  autolayer(test_air, series="Test data", size=1) +
  labs(y= NULL, color=NULL)

autoplot(fc1, include = 0) +
  autolayer(test_air, series="Test data", size=1) +
  labs(y= NULL, color=NULL)
