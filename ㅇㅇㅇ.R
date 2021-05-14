library(tidyverse)
library(forecast)
library(urca)


# 데이터 불러오기
birth <- read.csv("C:/Data/출생아 수.csv")
birth <- birth[-1,]
birth$전국 <- as.integer(birth$전국)
str(birth)
summary(birth)

# 시계열데이터 변환 & 분포확인
birth.ts <- ts(birth$전국,start=c(1997,1), freq=12)
birth.ts

autoplot(birth.ts)

ggseasonplot(birth.ts, year.labels=TRUE,year.labels.left = TRUE)

ggsubseriesplot(birth.ts)
data.frame(birth.ts=as.numeric(birth.ts), mon=as.factor(cycle(birth.ts))) %>% 
  ggplot() +
  geom_boxplot(aes(x=mon,y=birth.ts)) +
  labs(x="Month")

# train & test 나누기
train <- window(birth.ts , end=c(2017,12))
test <- window(birth.ts, start=c(2018,1))

# ETS 모델 적합 & 가정 검정
fit1 <- ets(train)
summary(fit1)

autoplot(fit1)
checkresiduals(fit1)

# 예측
fc1 <- forecast(fit1, h = length(test))
accuracy(fc1, test)


autoplot(train) +
  autolayer(test, series="Test data", size=1) +
  autolayer(fc1, series="ets", size=1,PI=FALSE) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc1, series="ets", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc1, series="ets", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)



## ARIMA모델
# 정상성만족 확인

train %>% ur.kpss() %>% summary()
ggtsdisplay(train)

# 비계절,계절 차분 확인
ndiffs(train)
nsdiffs(train)

train_d <- diff(train,lag=12)
ggtsdisplay(train_d) 
ggtsdisplay(diff(train_d))


# arima모형 적합
fit2 <- auto.arima(train,d=1,stepwise=FALSE,
                   approximation = FALSE,trace=TRUE)
fit3 <- auto.arima(train,d=1)


# 모델 가정 가설 검정 
checkresiduals(fit2)
checkresiduals(fit3)

# 예측
fc2 <- forecast(fit2)
fc3 <- forecast(fit3)

# 정확도 비교
accuracy(fc2, test)
accuracy(fc3, test)

# 예측 그래프

autoplot(test, series="Test data", size=1) +
  autolayer(fc2, series="ARIMA(0,1,4)(0,1,1)[12]", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)
  


autoplot(test, series="Test data", size=1) +
  autolayer(fc2, series="ARIMA(0,1,4)(0,1,1)[12]", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc3, series="ARIMA(2,1,2)(0,1,1)[12]", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc3, series="ARIMA(2,1,2)(0,1,1)[12]", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)


# 최종 모형
coef(fit3)

confint(fit3)

# ETS & ARIMA 비교
autoplot(test, series="Test data", size=1) +
  autolayer(fc1, series="ETS", size=1, alpha=0.3) +
  autolayer(fc3, series="ARIMA", size=1, alpha=0.3) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc1, series="ETS", size=1, PI = FALSE) +
  autolayer(fc3, series="ARIMA", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)

# 2021년 12월까지의 예측
fc4 <- forecast(fit3,h = 48)

autoplot(birth.ts,PI=FALSE) +
  autolayer(fc4,PI=F,col="red",size=0.8)
  labs(x=NULL,y=NULL, color=NULL)

