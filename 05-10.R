library(tidyverse)
library(fpp2)
library(forecast)

train_b <- window(ausbeer, start=1975, end=c(2008,2))
test_b <- window(ausbeer, start=c(2008,3))

time(train_b)
seasonaldummy(train_b)[1:5,]
fourier(train_b,K=2)[1:5,]

fit3 <- tslm(train_b ~ time(train_b) + fourier(train_b,K=2))
summary(fit3)

data.frame(data=train_b, fitted=fit1$fitted) %>%
  ggplot(aes(x=data,y=fitted)) +
  geom_point(aes(color=factor(cycle(data))), size=2) +
  geom_abline(intercept=0, slope=1) +
  labs(color="Quarter", x="Observed data", y="Fitted data")

Time <- time(train_b)
fit1 <- tslm(train_b ~ Time + seasonaldummy(train_b))
new1 <- data.frame(Time=time(test_b), seasonaldummy(test_b))

data.frame(data=train_b, fitted=fit1$fitted) %>%
  ggplot(aes(x=data,y=fitted)) +
  geom_point(aes(color=factor(cycle(data))), size=2) +
  geom_abline(intercept=0, slope=1) +
  labs(color="Quarter", x="Observed data", y="Fitted data")

fc1 <- forecast(fit1, newdata=new1)


fit2_2 <- tslm(train_b ~ trend + season)
fc2 <- forecast(fit2_2,h=8)

Time <- time(train_b)
Qtr <- factor(cycle(train_b))
model.matrix(~ Time + Qtr)[1:4,-1]

fit4 <- auto.arima(train_b,
                   xreg = cbind(time(train_b),
                                seasonaldummy(train_b)),
                   stepwise=FALSE, approximation=FALSE)

fit1
fit4
