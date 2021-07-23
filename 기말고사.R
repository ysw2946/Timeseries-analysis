library(tidyverse)
library(forecast)

Final <- read.csv("C:/Data/Final.csv")
Final.ts <- ts(Final[,3:4], start=c(2000,1),freq=12)

# 데이터 분포 확인
autoplot(Final.ts,facet=T)

# Elec에 변화폭이 점점 커지니 로그변환을 실시
autoplot((Final.ts[,1]))
autoplot(log(Final.ts[,1]))

# 데이터 분할
train <- window(Final.ts, end=c(2017,12))
test <- window(Final.ts, start=c(2018,1))


# ets모형
fit_ets <- ets(train[,1],lambda=0)
fit_ets

checkresiduals(fit_ets)

# arima
# 정상성확인
ggtsdisplay(log(train[,1]))

# 차분확인
train_l <- log(train[,1])
ndiffs(train_l)
nsdiffs(train_l)

d_train <- diff(train_l)
ggtsdisplay(d_train)
ggtsdisplay(diff(d_train,lag=12))

# arima모형적합
fit_arima <- auto.arima(train[,1],lambda=0)
fit_arima
checkresiduals(fit_arima)

# arma 오차회귀모형
# 설명변수 생성
TIME <- time(train[,1])
MONTH <- seasonaldummy(train[,1])

# Dummy 오차 회귀
new1 <- cbind(TIME,MONTH)
fit_arma1 <- auto.arima(train[,1], 
                     xreg=new1,lambda=0)
summary(fit_arma1)
checkresiduals(fit_arma1)

# k별 AICc 값
Time <- time(train[,1])
res <- vector("numeric",6)
for (i in seq(res)){
  xreg <- cbind(Time, fourier(train[,1],K=i))
  fit <- auto.arima(train[,1],lambda=0 ,xreg=xreg)
  res[i] <- fit$aicc
}
res
which.min(res)

# Fourier 오차 회귀
Time <- time(train[,1])
Fourier <- fourier(train[,1], K=5)
fit_arma2 <- auto.arima(train[,1],xreg=cbind(Time,Fourier),
                        lambda=0)
summary(fit_arma2)
checkresiduals(fit_arma2)




# Dynamic 회귀
fit_dy <- auto.arima(train[,1], lambda=0,
                   xreg=train[,2])
checkresiduals(fit_dy)

# 예측
# ets 예측
fc_ets <- forecast(fit_ets,h=length(test[,1]))



# arima예측
fc_arima <- forecast(fit_arima, h=length(test[,1]))

# ARMA 오차 회귀 예측
new3 <- cbind(Time = time(test[,1]),
              Fourier=fourier(test[,1], K=5))

fc_arma <- forecast(fit_arma2, xreg=new3)


# dynamic 예측
test[,2] <- mean(train[,2])
test

fc_dy <- forecast(fit_dy, xreg=test[,2])
fc_dy

# 예측비교
accuracy(fc_ets, test[,1])
accuracy(fc_arima, test[,1])
accuracy(fc_arma, test[,1])
accuracy(fc_dy, test[,1])

plot_ets <- autoplot(fc_ets,include=0,PI=0) +
  autolayer(test[,1]) +
  labs(x="year",y="elec")
plot_arima <- autoplot(fc_arima,include=0,PI=0) +
  autolayer(test[,1]) +
  labs(x="year",y="elec")
plot_arma <- autoplot(fc_arma,include=0,PI=0) +
  autolayer(test[,1]) +
  labs(x="year",y="elec")
plot_dy <- autoplot(fc_dy,include=0,PI=0) +
  autolayer(test[,1]) +
  labs(x="year",y="elec")

Rmisc::multiplot(plot_ets,plot_arima,plot_arma,
                 plot_dy)
