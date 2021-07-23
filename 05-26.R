library(tidyverse)
library(fpp2)
library(forecast)
library(patchwork)

head(elecdaily)
autoplot(elecdaily, facets=TRUE)

Demand <- elecdaily[,1]
Work <- elecdaily[,2]
Temp <- elecdaily[,3]

ggtsdisplay(Demand)
ggtsdisplay(Temp)

start(Demand)
end(Demand)
frequency(Demand)

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

autoplot(uschange, facets=TRUE)
checkresiduals(uschange)
GGally::ggpairs(as_tibble(uschange) %>% 
                  relocate(Consumption, .after=last_col()),
                lower=list(continuous="smooth"))

