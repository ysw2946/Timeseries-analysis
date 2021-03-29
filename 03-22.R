library(tidyverse)
library(fpp2)
library(ggplot2)
library(forecast)

train_hyn <- subset(hyndsight, end=length(hyndsight)-35)
test_hyn <- subset(hyndsight, start=length(hyndsight)-34)


fc1 <- hw(train_hyn, seasonal='additive',h=35)
fc2 <- hw(train_hyn, seasonal='multiplicative',h=35)
fc3 <- hw(train_hyn, damped=TRUE,seasonal='additive',h=35)
fc4 <- hw(train_hyn, damped=TRUE,seasonal='multiplicative',h=35)

autoplot(test_hyn, series="Test data") +
  autolayer(fc2, series="HW Multi", PI=FALSE) +
  autolayer(fc4, series="HW damped Multi", PI=FALSE) +
  ylab(NULL)
