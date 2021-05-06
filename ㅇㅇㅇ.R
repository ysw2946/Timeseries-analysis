library(tidyverse)
library(forecast)
library(urca)
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

## ARIMA모델

# 기본자료 차분 계절차분 확인
ggtsdisplay(a.ts)

# 로그변환
lam <- BoxCox.lambda(a.ts)
autoplot(BoxCox(a.ts,lam))
autoplot(BoxCox(a.ts,0))

# 차분확인
ggtsdisplay(diff(a.ts))
log(a.ts) %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
log(a.ts) %>% diff(lag=12) %>% length()

a.ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay()
a.ts %>% diff(lag=12) %>% length()


ndiffs(a.ts)
nsdiffs(a.ts)

# 정상성만족 확인
# kpss 테스트로 통계량값이 3.5876으로 유의수준 5%에서 0.463값보다 크기떄문에 
# 귀무가설을 기각하여 비정상모델임을 확인할 수 있따.
# 따라서 차분이 필요하다

a.ts %>% ur.kpss() %>% summary()
a.ts %>% diff(lag=12) %>% nsdiffs()

# arima모형 적합
auto.arima(a.ts, d=1)

# train & test 나누기
train <- window(a.ts , end=c(2017,12))
test <- window(a.ts, start=c(2018,1))

# train acf,pacf
ggtsdisplay(train)

# train 차분
train %>% diff(lag=12) %>% ggtsdisplay()

# train arima 적합
# INF가 존재 가역성에 문제가 있을 수 있음
fit1 <- auto.arima(train,d=0,trace=TRUE)
fit2 <- auto.arima(train,d=1,trace=TRUE)

# train 신뢰구간
# ma3에서 0이 포함되어있음
confint(fit1)
confint(fit2)

# 모델 가정 가설 검정 
checkresiduals(fit1)
checkresiduals(fit2)

# 예측
fc1 <- forecast(fit1)
fc2 <- forecast(fit2)

# 정확도 비교
accuracy(fc1, test)
accuracy(fc2, test)


# 그래프
autoplot(fc1,include=0) +
  autolayer(test, series="test")

autoplot(fc2,include=0) +
  autolayer(test, series="test")

# 최종모형
coef(fit2)


# 계절요인을 제거하고 분석
ad <- stl(a.ts,s.window="periodic") %>% seasadj()
autoplot(ad, series="ad") +
  autolayer(a.ts, series="raw")

train_d <- window(ad, end=c(2017,12))
test_d <- window(ad, start=c(2018,1))

ggtsdisplay(train_d)

train_d %>% diff() %>% ggtsdisplay()

