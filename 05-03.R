library(forecast)
library(tidyverse)
library(fpp2)

train_b <- window(ausbeer, start=1975, end=c(2008,2))
test_b <- window(ausbeer, start=c(2008,3))

time(train_b)
seasonaldummy(train_b)[1:5,]
fourier(train_b,K=2)[1:5,]


fit1 <- tslm(train_b ~ time(train_b) + seasonaldummy(train_b))
summary(fit1)

Time <- 1:length(train_b)
fit2 <- tslm(train_b ~ Time + seasonaldummy(train_b))
summary(fit2)

fit2_2 <- tslm(train_b ~ trend + season)
summary(fit2_2)
