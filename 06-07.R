library(tidyverse)
library(fpp2)
library(forecast)

auscafe_q <- auscafe %>%
  aggregate(nfrequency = 4)
auscafe
austourists
d <- ts.intersect(auscafe_q, austourists)
d

autoplot(d, facets = TRUE)

# 자료분리

d.tr <- head(d, n = nrow(d)-8)
d.te <- tail(d, n = 8)

cafe.tr <- d.tr[,1]
cafe.te <- d.te[,1]

tour.tr <- d.tr[,2]
tour.te <- d.te[,2]

autoplot(cbind(cafe.tr, log(cafe.tr)), facets = TRUE)

fit1 <- ets(cafe.tr)
checkresiduals(fit1)


log(cafe.tr) %>% ggtsdisplay()
log(cafe.tr) %>% ndiffs()
log(cafe.tr) %>% nsdiffs()

log(cafe.tr) %>% diff(lag=4) %>% ggtsdisplay
log(cafe.tr) %>% diff(lag=4) %>% ndiffs()


fit2 <- auto.arima(cafe.tr, lambda=0,
                   stepwise=F, approximation = F)
checkresiduals(fit2)


Time <- time(cafe.tr)
Quarter <- seasonaldummy(cafe.tr)

fit3 <- auto.arima(cafe.tr, lambda=0,
                   xreg=cbind(Time,Quarter),
                   stepwise=F,approximation = F)
checkresiduals(fit3)


fit4 <- auto.arima(cafe.tr, lambda=0,
                   xreg=tour.tr,
                   stepwise = F, approximation = F)
checkresiduals(fit4)


fc1 <- forecast(fit1, h=length(cafe.te))
fc2 <- forecast(fit2, h=length(cafe.te))
new.x <- cbind(Time = time(cafe.te),
               Quarter = seasonaldummy(cafe.te))
fc3 <- forecast(fit3, xreg=new.x)
fc4 <- forecast(fit4, xreg=tour.te)

p1 <-  autoplot(fc1,include=8) +
  autolayer(cafe.te)

Rmisc::multiplot(p1,p2,p3,p4)

accuracy(fc1, cafe.te)
accuracy(fc2, cafe.te)
accuracy(fc3, cafe.te)
accuracy(fc4, cafe.te)
