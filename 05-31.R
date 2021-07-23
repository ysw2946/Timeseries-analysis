library(tidyverse)
library(fpp2)
library(forecast)
library(patchwork)
library(lubridate)
wday(ymd("2014-1-1"), label=TRUE)

tibble(Demand, Temp)%>%
  ggplot(aes(x=Temp, y=Demand)) +
  geom_point() +
  geom_smooth(se=FALSE)

xreg <- cbind(Temp, Temp2 = Temp^2, Work)
fit <- auto.arima(Demand, xreg=xreg)
checkresiduals(fit)


old_T <- Temp[1:10]
new_x <- cbind(Temp=old_T, Temp2=old_T^2,
               work=c(0,1,0,0,1,1,1,1,1,0))

fc <- forecast(fit,xre=new_x)
autoplot(fc)

autoplot(uschange, facets=TRUE)
ggtsdisplay(uschange[,1])
ggtsdisplay(uschange[,2])
ggtsdisplay(uschange[,3])
ggtsdisplay(uschange[,4])
ggtsdisplay(uschange[,5])



GGally::ggpairs(as_tibble(uschange) %>% 
                  relocate(Consumption, .after=last_col()),
                lower=list(continuous="smooth"))

uschange_te <- tail(uschange, n=8)
uschange_tr <- head(uschange, n=nrow(uschange)-8)

fit1 <- auto.arima(uschange_tr[,1])
fit2 <- ets(uschange_tr[,1])
fit3 <- auto.arima(uschange_tr[,1],xreg=uschange_tr[,2:5])

fit3_1 <- Arima(uschange_tr[,1], xreg=uschange_tr[,2:5],
                order=c(3,1,0), seasonal=c(1,0,0),
                include.drift=FALSE)

checkresiduals(fit1)
checkresiduals(fit2)
checkresiduals(fit3)
checkresiduals(fit3_1)

fc1 <- forecast(fit1, h=8)
fc2 <- forecast(fit2, h=8)
fc3 <- forecast(fit3, xreg=uschange_te[,2:5])
fc3_1 <- forecast(fit3_1,xreg=uschange_te[,2:5])

accuracy(fc1,uschange_te[,1])
accuracy(fc2,uschange_te[,1])
accuracy(fc3,uschange_te[,1])
accuracy(fc3_1,uschange_te[,1])

autoplot(uschange_te[,1],series="test",size=1.5) +
  autolayer(fc1, series="arima",PI=F,size=1.5) +
  autolayer(fc2, series="ets",PI=F,size=1.5) +
  autolayer(fc3, series="dynamic",PI=F,size=1.5) +
  ylab(NULL)

p1 <- autoplot(fc1) + autolayer(uschange_te[,1]) +
  theme(legend.position = "none") + ylab(NULL)
p2 <- autoplot(fc2) + autolayer(uschange_te[,1]) +
  theme(legend.position = "none") + ylab(NULL)
p3 <- autoplot(fc3) + autolayer(uschange_te[,1]) +
  theme(legend.position = "none") + ylab(NULL)
p3_1 <- autoplot(fc3_1) + autolayer(uschange_te[,1]) +
  theme(legend.position = "none") + ylab(NULL)
Rmisc::multiplot(p1,p2,p3,p3_1, cols=2)


Quotes <- insurance[,1]
Ad <- insurance[,2]
Ad_1 <- stats::lag(Ad,-1)
Ad_2 <- stats::lag(Ad,-2)
Ad_3 <- stats::lag(Ad,-3)

start(Ad_1)
start(Ad_2)
start(Ad_3)
start(Quotes)
start(Ad)

d1 <- ts.intersect(Quotes,Ad,Ad_1,Ad_2,Ad_3)
d1
