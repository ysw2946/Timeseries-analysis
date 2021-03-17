library(tidyverse)
library(forecast)
library(ggplot2)
library(fpp2)

elecequip

# 
decompose(elecequip) %>%
  autoplot() + labs(x="Year")


# s.window 효과
stl(elecequip, s.window="periodic") %>% autoplot()

stl(elecequip, s.window=5) %>% autoplot()

# t.widnow 효과

stl(elecequip, s.window="periodic", t.window = 11) %>%
  autoplot()

stl(elecequip, s.window="periodic", t.window = 7) %>%
  autoplot()


mstl(elecequip) %>% autoplot()

