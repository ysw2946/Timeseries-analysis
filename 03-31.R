library(tidyverse)
library(forecast)
library(fpp2)
library(fma)

traind <- window(dole,end = c(1991,7))
testd <- window(dole, start = c(1991,8))


# 아웃라이어들 처리가 중요
fit <- ets(traind)

# 독립성 가정 위반
# point forecast에는 큰 문제가 없으나,
# prediction interval에 대한 신뢰도에 문제가 있을 수 있음
autoplot(fit)
checkresiduals(fit)

dc <- forecast(fit, h = length(testd))
accuracy(dc,testd)

autoplot(dc) +
  autolayer(testd,series="testd",size=1) +
  labs(y=NULL,col=NULL)

autoplot(dc,include = 0) +
  autolayer(testd,series="testd",size=1) +
  labs(y=NULL,col=NULL)


train_cafe <- subset(auscafe, end=length(auscafe)-24)
test_cafe <- subset(auscafe, start=length(auscafe)-23)

ets_fc <- train_cafe %>%
  ets() %>% forecast()

ets_bag <- baggedModel(train_cafe)

ets_fc$model$method

ets_model <- vector("character", 100)
for(i in seq(ets_model)){
  ets_model[i] <- ets_bag$models[[i]]$method
}
table(ets_model)
