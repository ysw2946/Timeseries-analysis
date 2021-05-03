library(tidyverse)
library(forecast)
library(fpp2)

elecequip_desea <- stl(elecequip, s.window="periodic") %>%
  seasadj()

autoplot(elecequip_desea, series="Seasonally adjusted",size=1) + 
  autolayer(elecequip, series="Monthly data", size=1)

train_eq <- window(elecequip_desea, end=c(2010,3))
test_eq <- window(elecequip_desea, start=c(2010,4))

ggtsdisplay(train_eq)
ndiffs(train_eq)

fit1 <- auto.arima(train_eq, stepwise=FALSE,
                   approximation = FALSE, seasonal=FALSE)
fit1

checkresiduals(fit1)

fc1 <- forecast(fit1)
accuracy(fc1, test_eq)

autoplot(fc1, include=20) +
  autolayer(test_eq, series="test data", size=1) +
  theme(legend.position="none") +
  ylab("Electrical equipment manufactured")


depart <- scan("C:/Data/depart.txt")
depart.ts <- ts(depart, start=1984, freq=12)

autoplot(depart.ts)

lndepart <- log(depart.ts)
autoplot(lndepart)
ggtsdisplay(lndepart, lag.max=36)
ndiffs(lndepart)
nsdiffs(lndepart)

lndepart_12 <- diff(lndepart, lag=12)
ggtsdisplay(lndepart_12)
lndepart_12_1 <- diff(lndepart_12)
ggtsdisplay(lndepart_12_1)

fit1 <- auto.arima(depart.ts,lambda=0,
                   stepwise=FALSE, approximation=FALSE)
fit1
checkresiduals(fit1)

fc1 <- forecast(fit1)
autoplot(fc1)
