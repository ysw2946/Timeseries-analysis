library(tidyverse)
library(ggplot2)
library(forecast)

AP <- AirPassengers
AP

class(AP)

start(AP); end(AP); frequency(AP)

depart <- scan("C:/Data/depart.txt",fileEncoding = "UTF-8-BOM")
depart.ts <- ts(depart, 1984,freq=12)
depart.ts

#install.packages("ggplot2")
#install.packages("forecast")

autoplot(depart.ts) +
  labs(title="Monthly sales of a department store",
       x="Year", y="")

global <- scan("C:/Data/global.txt",fileEncoding="UTF-8-BOM")
global.ts <- ts(global, start=1856,freq =12)
autoplot(global.ts) +
  labs(title="Global Temperature 1985 ~ 2005", x="Year",
       y="")

global.1970 <- window(global.ts, start=1970)
autoplot(global.1970) +
  labs(title="Global Temperature 1970 ~ 2005",x="Year",y="")

autoplot(global.1970) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Global Temperature 1970 ~ 2005", x="Year",y="")

### Seasonal graph
AP <- AirPassengers
ggseasonplot(AP)

ggseasonplot(AP, year.labels=TRUE, year.labels.left=TRUE)
ggseasonplot(AP,polar=TRUE)
ggsubseriesplot(AP)

data.frame(AP=as.numeric(AP),mon=as.factor(cycle(AP))) %>%
  ggplot()+
  geom_boxplot(aes(x=mon,y=AP)) +
  labs(x="Month")

library(fpp2)
ma(elecsales,5)

ma(window(ausbeer, start = 2007), 4)


# legend와 color를 넣고싶으면 autolayer 사용
autoplot(elecsales, series="Data") +
  autolayer(ma(elecsales,3), series="3-MA") +
  scale_color_manual(values=c("Data"="blue", "3-MA"="red"))


autoplot(ausbeer, series="Data") +
  autolayer(ma(ausbeer,12), series="12-MA") +
  scale_color_manual(values=c("Data"="blue", "12-MA"="red"))

