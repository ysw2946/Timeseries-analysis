library(tidyverse)
library(fpp2)
library(ggplot2)
library(forecast)

# ausair
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



# austourists
train_aus <- window(austourists, end=c(2013,4))
test_aus <- window(austourists, start = c(2014,1))

fit <- ets(train_aus)
fit

autoplot(fit)
checkresiduals(fit)

fit2_1 <- ets(train_aus, lambda = 0)
fit2_1

autoplot(fit2_1)
checkresiduals(fit2_1)


fc2 <- forecast(fit, h = length(test_aus))
accuracy(fc2, test_aus)

fc3 <- forecast(fit2_1, h = length(test_aus))
accuracy(fc3, test_aus)


autoplot(train_aus) +
  autolayer(test_aus, series = "Test Data", size=1) +
  autolayer(fc2, series = "ETS(MAM)", size=1) +
  autolayer(fc3, series = "ETS(AAA)", size=1) +
  labs(y=NULL, color=NULL)

autoplot(fc3, include=0) +
  autolayer(test_aus, series = "Test Data", size=1) +
  labs(y=NULL, color=NULL)
