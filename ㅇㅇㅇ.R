library(tidyverse)
library(forecast)

# 데이터 불러오기
a <- read.csv("C:/Data/출생아 수.csv")
a <- a[-1,]
a$전국 <- as.integer(a$전국)
str(a)
summary(a)

# 시계열데이터 변환 & 분포확인
a.ts <- ts(a$전국,start=c(1997,1), freq=12)
a.ts

autoplot(a.ts)

ggseasonplot(a.ts, year.labels=TRUE,year.labels.left = TRUE)

ggsubseriesplot(a.ts)
ggtsdisplay(a.ts)
data.frame(a.ts=as.numeric(a.ts), mon=as.factor(cycle(a.ts))) %>% 
  ggplot() +
  geom_boxplot(aes(x=mon,y=a.ts)) +
  labs(x="Month")

# train & test 나누기
train <- window(a.ts , end=c(2017,12))
test <- window(a.ts, start=c(2018,1))

# ETS 모델 적합 & 가정 검정
fit <- ets(train)
summary(fit)

autoplot(fit)
checkresiduals(fit)

# 예측
fc <- forecast(fit, h = length(test))
accuracy(fc, test)


autoplot(train) +
  autolayer(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1,PI=FALSE) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc, series="ets", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)

