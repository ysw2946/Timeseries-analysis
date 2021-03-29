library(tidyverse)
library(fpp2)
library(ggplot2)
library(forecast)

ggAcf(oil)
ggPacf(oil)

ggtsdisplay(AirPassengers, lag.max=48)
