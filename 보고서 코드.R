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
fit_ets <- ets(train)
summary(fit_ets)

autoplot(fit_ets)
checkresiduals(fit_ets)

# 예측
fc_ets <- forecast(fit_ets, h = length(test))
accuracy(fc_ets, test)


autoplot(train) +
  autolayer(test, series="Test data", size=1) +
  autolayer(fc_ets, series="ets", size=1,PI=FALSE) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc_ets, series="ets", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc_ets, series="ets", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)



###### ARIMA모델
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

fit_arima <- fit3
fc_arima <- fc3



##### ARMA 오차 회귀 모델
# 설명변수 생성
TIME <- time(train)
MONTH <- seasonaldummy(train)

# 회귀모델 적합
fit_g1 <- tslm(train ~ TIME + MONTH)
summary(fit_g1)


# 가정만족 여부 확인
checkresiduals(fit_g1)
ggtsdisplay(fit_g1$residuals)

# 회귀모델 적합(백색잡음 가정)
new_g1 <- data.frame(TIME =time(test), MONTH = seasonaldummy(test))
fc_g1 <- forecast(fit_g1, newdata=new_g1)

# ARMA 오차 회귀 모델1 적합
new1 <- cbind(TIME,MONTH)
fit_g2 <- auto.arima(train, 
                     xreg=new1,
                     stepwise=F,approximation=F)
summary(fit_g2)

checkresiduals(fit_g2)

# ARMA 오차 회귀 모델1 예측
new_g2 <- cbind(Time = time(test), MONTH = seasonaldummy(test))

fc_g2 <- forecast(fit_g2, xreg=new_g2)


# k별 AICc 값
Time <- time(train)
res <- vector("numeric",6)
for (i in seq(res)){
  xreg <- cbind(Time, fourier(train,K=i))
  fit <- auto.arima(train,xreg=xreg)
  res[i] <- fit$aicc
}
res
(k_min <- which.min(res))

# ARMA 오차 회귀 모델2 적합
Time <- time(train)
Fourier <- fourier(train, K=k_min)
fit_g3 <- auto.arima(train,xreg=cbind(Time,Fourier),
                     stepwise = F, approximation = F)
summary(fit_g3)

checkresiduals(fit_g3)

# ARMA 오차 회귀 모델2 예측
new_g3 <- cbind(Time = time(test),
                Fourier=fourier(test, K=k_min))

fc_g3 <- forecast(fit_g3, xreg=new_g3)



# 백색잡음 & ARMA 오차 회귀 모델 예측 그래프
autoplot(test, series="Test data", size=1) +
  autolayer(fc_g3, series="ARMA", size=1, alpha=0.5) +
  labs(y= NULL, color=NULL)


autoplot(test, series="Test data", size=1) +
  autolayer(fc_g3, series="ARMA", size=1, PI = FALSE) +
  labs(y= NULL, color=NULL)


### 모델별 test와의 오차 비교

accuracy(fc_ets, test)
accuracy(fc_arima,test)
accuracy(fc_g3,test)


## 모델별 그래프 (신뢰구간 포함)
p1 <- autoplot(fc_ets, include=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL)

p2 <- autoplot(fc_arima, include=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) 

p3 <- autoplot(fc_g3, include=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) 

Rmisc::multiplot(p1,p2,p3)

## 모델별 그래프 (신뢰구간 제거)

p1 <- autoplot(fc_ets, include=0,PI=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL)

p2 <- autoplot(fc_arima, include=0,PI=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) 

p3 <- autoplot(fc_g3, include=0,PI=0) +
  autolayer(test, size=1) +
  theme(legend.position = "none") +
  labs(x=NULL,y=NULL) 

Rmisc::multiplot(p1,p2,p3)

### 2021년 12월까지의 예측
fc4 <- forecast(fit_arima,h = 48)

autoplot(birth.ts) +
  autolayer(fc4,PI=F,col="red",size=0.8) +
  labs(x=NULL,y=NULL, color=NULL)

