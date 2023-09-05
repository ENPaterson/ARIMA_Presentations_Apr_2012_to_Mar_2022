.libPaths("//hscni.net/bso/Honest Broker Service/R packages")
.libPaths()

library(ggplot2)
library(tidyverse)
library(haven)
library(expsmooth)
library(tibble)
library(tidyr)
library(stringr)
library(timeDate)
library(tseries)
library(zoo)
library(xts)
library(fracdiff) 
library(lmtest)
library(rpart)
library(splines)
library(boot)
library(dplyr)
library(readxl)
ARIMA <- read_excel("//hscni.net/bso/Honest Broker Service/Project 060/ENPaterson files/Self-harm M3 2021 data/M3 ARIMA.xlsx")

str(ARIMA)

ARIMA  %>% 
  filter(c_month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) %>%
  ggplot(aes(x=as.factor(year), y=rural_presentations_n))+
  geom_bar(stat="identity")+
  facet_wrap(~c_month)+
  labs(
    x = "Month",
    y = "Presentations (n)"
  )+
  theme(axis.text.x = element_text(angle = 90))

ARIMA %>% 
  ggplot(aes(x=month, y=rural_presentations_n))+
  geom_line()+
  labs(
    y = "Presentations (n)"
  )

ARIMA  %>% 
  filter(year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x=c_month, y=rural_presentations_n))+
  geom_line()+
  facet_wrap(~year)+
  labs(
    x = "Month",
    y = "Presentations (n)"
  )+
  theme(axis.text.x = element_text(angle = 90))

Apr2012_Feb2022 <- ARIMA$rural_presentations_n[1:95]      

PANDEMIC2020_2022 <- ARIMA$rural_presentations_n[96:120]   

Apr2012_Feb2022_ts <- ts(Apr2012_Feb2022, 
                         start=c(2012,4),  #first in time series is Apr 2012
                         end=c(2020,2),    #last in time series is Feb 2020
                         freq=12)          #i.e. monthly data

autoplot(Apr2012_Feb2022_ts) +  ylab("Presentations (n)")

diff_Apr2012_Feb2022 <- diff(Apr2012_Feb2022_ts)

autoplot(diff_Apr2012_Feb2022) +
  ylab("Presentations (n)")

ggseasonplot(diff_Apr2012_Feb2022) + ylab("Presentations (n)")

ggsubseriesplot(diff_Apr2012_Feb2022) +  ylab("Presentations (n)")

set.seed(123)
fit_naive_Apr2012_Feb2022 <-snaive(diff_Apr2012_Feb2022)  
summary(fit_naive_Apr2012_Feb2022)

checkresiduals(fit_naive_Apr2012_Feb2022)

set.seed(123)
fit_ets_Apr2012_Feb2022<- ets(Apr2012_Feb2022_ts)
summary(fit_ets_Apr2012_Feb2022)
checkresiduals(fit_ets_Apr2012_Feb2022)

set.seed(123)
fit_arima_Apr2012_Feb2022 <- auto.arima(Apr2012_Feb2022_ts,       
                                        d=1,               # d1 to take first difference of data to remove trend
                                        D=1,               # D1 to take the first seasonal difference to remove seasonal trends
                                        stepwise = F,      # if true, the algorithm would try a smaller number to save time (costs accuracy)
                                        approximation = F, # if true, the AIC would be an approximation to save time (costs accuracy)
                                        trace = T          # prints models as they run
)

summary(fit_arima_Apr2012_Feb2022)
checkresiduals(fit_arima_Apr2012_Feb2022)

forecast_arima_Apr2012_Feb2022 <- forecast(fit_arima_Apr2012_Feb2022,
                                           h=25) 
autoplot(forecast_arima_Apr2012_Feb2022) 

summary(forecast_arima_Apr2012_Feb2022)

forecast_arima_Apr2012_Feb2022_df <- data.frame(forecast_arima_Apr2012_Feb2022$mean)
lower_Apr2012_Feb2022_df <- as.data.frame(forecast_arima_Apr2012_Feb2022$lower,row.names = FALSE)
upper_Apr2012_Feb2022_df <- as.data.frame(forecast_arima_Apr2012_Feb2022$upper,row.names = FALSE)
PANDEMIC2020_2022_df <- as.data.frame(PANDEMIC2020_2022)
forecast_arima_Apr2012_Feb2022_df 

lower_Apr2012_Feb2022_df

upper_Apr2012_Feb2022_df

PANDEMIC2020_2022_df

forecast_arima_Apr2012_Feb2022_df <- cbind(forecast_arima_Apr2012_Feb2022_df, lower_Apr2012_Feb2022_df, upper_Apr2012_Feb2022_df, PANDEMIC2020_2022_df)

column_names <-  c("mean", "lower80", "lower95", "upper80", "upper95", "actual")

colnames(forecast_arima_Apr2012_Feb2022_df) <- column_names

month <- c("Mar2020", "Apr2020", "May2020", "Jun2020", "Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021", "Mar2021", "Apr2021", "May2021", "Jun2021", "Jul2021", "Aug2021", "Sep2021", "Oct2021", "Nov2021", "Dec2021", "Jan2022", "Feb2022", "Mar2022")
forecast_arima_Apr2012_Feb2022_df <- forecast_arima_Apr2012_Feb2022_df %>%
  add_column(month, .before = "mean")

forecast_arima_Apr2012_Feb2022_df$month <- factor(forecast_arima_Apr2012_Feb2022_df$month, levels = c("Mar2020", "Apr2020", "May2020", "Jun2020", "Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021", "Mar2021", "Apr2021", "May2021", "Jun2021", "Jul2021", "Aug2021", "Sep2021", "Oct2021", "Nov2021", "Dec2021", "Jan2022", "Feb2022", "Mar2022"))

forecast_arima_Apr2012_Feb2022_df

ggplot(data = forecast_arima_Apr2012_Feb2022_df,
       aes(x= month, y=mean, group=1))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin= lower95, ymax= upper95), linetype=2, alpha=0.1)+
  geom_ribbon(aes(ymin= lower80, ymax= upper80), linetype=2, alpha=0.2)+
  geom_point(aes(x= month, y= actual, group=1, col="red"))+
  geom_line(aes(x= month, y= actual, group=1, col="red"))+
  labs(
    caption = paste("Black = forecast values\nRed = actual values\nDark grey band = 80% confidence interval\nLight grey band = 95% confidence interval"),    x = "Month",
    y = "Presentations (n)"
  )+
  theme_minimal()+
  theme(legend.position = "none") +  
  ylim(0, 850)
ggsave("M4_7c.pdf", width = 15, height = 15)


