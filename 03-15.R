library(tidyverse)
library(fpp2)
library(ggplot2)
library(forecast)

# Simple exponential smoothing
oil
oil_1996 <- window(oil, start=1996)
autoplot(oil_1996)

fit_1 <- ses(oil_1996, h=3)
summary(fit_1)

autoplot(fit_1)


# Holt's linear trend model
# Damped Holt's trend model
air <- window(ausair, start=1990)

fit_2 <- holt(air, h=15)
fit_3 <- holt(air, h=15, damped=TRUE)
fit_4 <- holt(air, h = 15, phi = 0.8 ,damped = TRUE)

autoplot(air) +
  autolayer(fit_2, series="Holt's method", PI=FALSE) +
  autolayer(fit_3, series="Damped method", PI=FALSE) +
  autolayer(fit_4, series="PI Damped method", PI=FALSE)


summary(fit_3)
summary(fit_4)
autoplot(air) +
  autolayer(fit_3, series="Damped method")

autoplot(air) +
  autolayer(fit_4, series="PIDamped method")
