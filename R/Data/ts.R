rm(list=ls(all=TRUE))
setwd("C:/Users/admin/Desktop/RegularBatch")


data(mtcars)
names(mtcars)
summary(lm(mpg~disp+wt+qsec, 
           data=mtcars))

summary(lm(mpg~., 
           data=mtcars))


kings <- scan("http://robjhyndman.com/
              tsdldata/misc/kings.dat",
              skip=3)
kings

kingstimeseries <- ts(kings)
kingstimeseries


#For monthly time series data, 
#you set frequency=12, 
#while for quarterly time series data, 
#you set frequency=4

#You can also specify the first 
#year that the data was collected, 
#and the first interval in that year 
#by using the 'start'
#parameter in the ts() function. For example, if the first data 
#point corresponds to the second quarter of 1986, you would set 
#start=c(1986,2).

#a data set of the number of births per month in New York city, 
#from January 1946 to December 1959

births <- scan("http://robjhyndman.com/
               tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, 
                       frequency=12, 
                       start=c(1946,1))
birthstimeseries

#Similarly, the file http://robjhyndman.com/
#tsdldata/data/fancy.dat 
#contains monthly sales for a souvenir 
#shop at a beach resort town 
#in Queensland, Australia, for January 
#1987-December 1993 (original 
#data from Wheelwright
                                                                            and Hyndman, 1998). We can read the data into R by typing:
souvenir <- scan("http://robjhyndman.com/
                 tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, 
                         frequency=12, 
                         start=c(1987,1))
souvenirtimeseries

#Time series of the annual diameter of women's 
#skirts at the hem, 
#from 1866 to 1911. The data is available in 
#the file 
#http://robjhyndman.com/tsdldata/roberts/skirts.dat 
#(original data from Hipel and McLeod, 1994).

skirts <- scan("http://robjhyndman.com/
               tsdldata/roberts/skirts.dat",
               skip=5)
skirtsseries <- ts(skirts,start=c(1866))

#Plotting time series

plot.ts(kingstimeseries)
acf(kingstimeseries, lag.max=20)

plot.ts(birthstimeseries)
acf(birthstimeseries, 
    lag.max=20)

plot.ts(souvenirtimeseries)
acf(souvenirtimeseries, 
    lag.max=20)

plot.ts(skirtsseries)
acf(skirtsseries, 
    lag.max=20)

library("TTR")

kingstimeseriesSMA3 <- 
  SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

par(mfrow = c(1, 2))
plot.ts(kingstimeseriesSMA3)
plot.ts(kingstimeseriesSMA8)

par(mfrow = c(1, 1))

#Trend, seasonal, random

birthstimeseriescomponents <- 
  decompose(birthstimeseries)

plot(birthstimeseriescomponents)
birthstimeseriescomponents$seasonal

birthstimeseriesseasonallyadjusted <- 
  birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

birthsforecast <- 
  HoltWinters(birthstimeseries, 
              beta=FALSE, 
              gamma=FALSE)
birthsforecast
birthsforecast$fitted

plot(birthsforecast)
birthsforecast$SSE

library("forecast")
birthsforecast2 <- 
  forecast.HoltWinters(birthsforecast, h=8)
birthsforecast2

plot.forecast(birthsforecast2)

#Let us now 
#assume there is no 
#seasonality, but there 
#is trend

birthsforecast3 <- 
  HoltWinters(birthstimeseries, 
              gamma=FALSE)
birthsforecast3
plot(birthsforecast3)

#We can specify the first 
#value and slope

birthsforecast3 <- HoltWinters(birthstimeseries, 
                              gamma=FALSE, 
                              l.start=26.663, 
                              b.start=-3.065)
plot(birthsforecast3)

birthsforecast3 <- 
  forecast.HoltWinters(birthsforecast3, 
                       h=5)
plot.forecast(birthsforecast2, shadecols=terrain.colors(3))

#Additive, trend and seasonality models
logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- 
  HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
plot(souvenirtimeseriesforecasts)

#it predicts seasonal peaks well

souvenirtimeseriesforecasts2 <- 
  forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)

plot.forecast(souvenirtimeseriesforecasts2,
              shadecols="oldstyle")

plot.ts(souvenirtimeseriesforecasts2$residuals) # make a time plot


#ARIMA
plot(skirtsseries)
skirtsseriesdiff1 <- 
  diff(skirtsseries, 
       differences=1)
plot.ts(skirtsseriesdiff1)

skirtsseriesdiff2 <- 
  diff(skirtsseries, 
       differences=2)
plot.ts(skirtsseriesdiff2)

auto.arima(skirtsseries)

#Parsimonious models
skirtstimeseries <- 
  auto.arima(skirtsseries,
             ic='bic')
skirtstimeseries
skirtstimeseriesforecasts <- 
  forecast.Arima(skirtstimeseries, 
                 h=5)
plot.forecast(skirtstimeseriesforecasts)

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
kingstimeseriesarima <- 
  auto.arima(kingstimeseries)

kingstimeseriesarima <- 
  auto.arima(kingstimeseries,
             ic='bic')

kingstimeseriesforecasts <- 
  forecast.Arima(kingstimeseriesarima, 
                 h=5)
plot.forecast(kingstimeseriesforecasts)

