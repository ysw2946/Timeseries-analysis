library(tidyverse)
library(forecast)
library(readxl)
library(fpp2)
library(urca)


y <- ts(rnorm(100))

autoplot(y)
ggtsdisplay(y)

autoplot(elec)
lam <- BoxCox.lambda(elec)
autoplot(BoxCox(elec,lam))
autoplot(BoxCox(elec,0))

ggtsdisplay(goog200)
ggtsdisplay(diff(goog200))


autoplot(elec)
elec %>% log() %>%
  diff(lag=12) %>% diff() %>% autoplot() +
  labs(title="Log transformed and Doubly differenced data",y="")

ggtsdisplay(diff(BoxCox(elec,lelec)))

ndiffs(goog200)
goog200 %>% ur.kpss() %>% summary()

ndiffs(elec)
elec %>% ur.kpss() %>% summary()
