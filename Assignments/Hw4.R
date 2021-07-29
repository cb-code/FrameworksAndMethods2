###------------------------------------Assignment 4------------------------------------###
###------------------------------------chb2132------------------------------------###

library(gridExtra);
library(ggplot2);
library(ggthemes);
library(quantmod);
library(xts);
library(zoo);
library(forecast);
library(fpp);
library(fpp2);
library(tseries);
library(dplyr);

setwd("~/Documents/R/Assignment 4");

RNGversion(vstr = 3.6);

###------------------------------------Section 4.1------------------------------------###

goog = readRDS('goog.RDS');

sample.xts <- as.xts(goog)

class(sample.xts)
str(sample.xts)

head(sample.xts)  # attribute 'descr' hidden from view

sample.xts['2010-06']  # March 2007 to the end of the data set
sample.xts['2010']  # all of 2007

263.2578+261.6979+281.7276+261.1515+241.2459+
    221.0374+240.8584+223.5560+261.1962+304.8671+276.0595+295.0659;

3131.721/12;

start(goog);
end(goog);
time(goog);

### acf(x = goog, lag.max = 1, plot = F);

lag_1 <- goog/stats::lag(goog)-1;

cor(goog, stats::lag(goog)-1, use = 'complete.obs');
cor(goog, lag_1, use = 'complete.obs');

google = ts(data=goog,start=c(2007,01),frequency=12);
google;

train = window(google,end=c(2015,12));
test = window(google,start=c(2016,01));

length(test);

time(train);

9*12;

train_Acf <- ggAcf(train, type = c("correlation"));

#### sample.xts['2007-03/2007']  # March 2007 to the end of 2007 #### 
#### sample.xts['/'] # the whole data set #### 
#### sample.xts['/2007'] # the beginning of the data through 2007 #### 
#### sample.xts['2007-01-03'] # just the 3rd of January 2007 ####

plot(goog);
warnings();

autoplot(goog);
goog;

class(goog);

start(goog);
end(goog);
time(goog);

deltat(goog);

index(goog);
tclass(goog);

is.ts(goog);
### FALSE, is xts object and zoo (???) ###

###------------------------------------Section 4.2------------------------------------###

nmonths(test);

average_model = meanf(train, h = 34);

average_model;

average_model$mean;

window(average_model$mean, c(2018, 10))

accuracy(average_model); 

accuracy(average_model, x = google);

### fixed, use train when actually looking at test idk why tbh ###

goog;

accuracy(average_model, x = google);

naive_model = naive(train, h = 34);
naive_model$mean

accuracy(naive_model, x = google);

###------------------------------------Section 4.3------------------------------------###

ets_aaa <- ets(train,model = 'AAA');

summary(ets(train,model = 'AAA'));

checkresiduals(ets_aaa);

ets_aaa_forecast <- forecast(ets_aaa, h = 34);

ets_aaa_forecast;

accuracy(ets_aaa_forecast, x = google);

###------------------------------------Section 4.4------------------------------------###

auto_arima_model <- auto.arima(train);

?auto.arima;

ggAcf(train);
Acf(train);
checkresiduals(auto_arima_model);

no_seas <- auto.arima(train, seasonal = FALSE);
checkresiduals(no_seas);

ya_seas <- auto.arima(train, seasonal = TRUE);
checkresiduals(ya_seas);

autoplot(forecast(auto_arima_model, h = 34), PI = F)+autolayer(test, size = 1);
aam_forecast = forecast(auto_arima_model, h = 35);
accuracy(object = aam_forecast, x = google);
aam_forecast;

BoxCox.lambda(train);

arima_model <- Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train));
arima_model;

checkresiduals(arima_model);

autoplot(forecast(arima_model, h = 34), PI = F)+autolayer(test, size = 1);
am_forecast = forecast(arima_model, h = 35);
accuracy(object = am_forecast, x = google);
am_forecast;

###-----------------------------------END OF FILE-------------------------------------###
