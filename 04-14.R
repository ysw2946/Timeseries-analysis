library(tidyverse)
library(forecast)
library(fpp2)

gas <- read_csv("C:/Data/gas.csv")
gas %>% print(n=3)
rate.ts <- as.ts(gas$rate)
train_r <- window(rate.ts,end=length(rate.ts)-10)
test_r <- window(rate.ts,start=length(rate.ts)-9)

ggtsdisplay(train_r)
ndiffs(train_r)

fit1 <- auto.arima(train_r,d=0, stepwise=FALSE,
                   approximation=FALSE, seasonal=FALSE,
                   trace=TRUE)
fit1
confi

fit2 <- auto.arima(train_r, stepwise=FALSE,
                   approximation=FALSE, seasonal=FALSE,
                   trace=TRUE)
fit2

checkresiduals(fit1)
checkresiduals(fit2)

fc1 <- forecast(fit1)
fc2 <- forecast(fit2)

accuracy(fc1,test_r)
accuracy(fc2,test_r)

autoplot(fc1, include=0)+
  autolayer(test_r, series="Test data", size=1) +
  theme(legend.position = "none")+
  labs(y="rate")

elecequip_desea <- stl(elecequip, s.window="periodic") %>%
  seasadj()

autoplot(elecequip_desea, series="Seasonally adjusted",size=1) + 
  autolayer(elecequip, series="Monthly data", size=1)

train_eq <- window(elecequip_desea, end=c(2010,3))
test_eq <- window(elecequip_desea, start=c(2010,4))

ggtsdisplay(train_eq)
ndiffs(train_eq)
