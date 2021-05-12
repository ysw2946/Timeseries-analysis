library(tidyverse)
library(fpp2)
library(forecast)

train_b <- window(ausbeer, start=1975, end=c(2008,2))
test_b <- window(ausbeer, start=c(2008,3))


Time <- time(train_b)
fit1 <- tslm(train_b ~ Time + seasonaldummy(train_b))
new1 <- data.frame(Time=time(test_b), seasonaldummy(test_b))

data.frame(data=train_b, fitted=fit1$fitted) %>%
  ggplot(aes(x=data,y=fitted)) +
  geom_point(aes(color=factor(cycle(data))), size=2) +
  geom_abline(intercept=0, slope=1) +
  labs(color="Quarter", x="Observed data", y="Fitted data")

fc1 <- forecast(fit1, newdata=new1)

time(train_b)
Time <- time(train_b)
Qtr <- factor(cycle(train_b))
model.matrix(~ Time + Qtr)[1:4,-1]

fit4 <- auto.arima(train_b,
                   xreg = cbind(time(train_b),
                                seasonaldummy(train_b)),
                   stepwise=FALSE, approximation=FALSE)

fit1
fit4

fc1$upper
checkresiduals(fit4)

fc4 <- forecast(fit4, 
                xreg = cbind(time(test_b),
                                   seasonaldummy(test_b)))

fc1
fc4
fc1$upper[,2] - fc1$lower[,2]
fc4$upper[,2] - fc4$lower[,2]

accuracy(fc1,test_b)
accuracy(fc4,test_b)

autoplot(fc1,include=0) +
  autolayer(test_b, series="test")

autoplot(fc4,include=8) +
  autolayer(test_b, series="test") +
  theme(legend.position="none") +
  ylab(NULL)

global <- read.table("C:/Data/global.txt")
global <- global %>% as.list() %>% unlist()
global.ts <- ts(global, start=c(1970,1),freq=12)

autoplot(global.ts)

temp <- window(global.ts, start=1970)
train_g <- window(temp, end=c(2003,12))
test_g <- window(temp, start=c(2004,1))


TIME <- time(train_g)
MONTH <- seasonaldummy(train_g)
fit1 <- tslm(train_g ~ TIME + MONTH)
summary(fit1)

checkresiduals(fit1)
