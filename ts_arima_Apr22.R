rm(list=ls(all=TRUE))
setwd("C:/Users/SahilMattoo/Desktop/Udemy/TimeSeries/GLAKES/ARIMA")

library("forecast")
library("stats")
library("data.table")

## US Air carrier traffic statistics data (revenue passenger miles)
data = read.csv("us-air-carrier-traffic.csv", header = T)
class(data)
head(data)
attach(data)
par(mfrow = c(1, 1))
plot(miles, type = "l")
miles.ts = ts(miles, start = c(1996,1), frequency = 12)
plot(miles.ts, xlab = "US Air Carrier Traffic Statistics: Revenue passenger miles")

milestimeseries = miles.ts

## DIFF
miles.d = diff(miles)
plot(miles.d, type = 'l')

miles.d2 = diff(miles.d)
plot(miles.d2, type = 'l')

#ARIMA
#Step-by-step ARIMA model building
# Model 1
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
par(mfrow = c(1, 1))
plot(milestimeseries)

# Step 2: Plot ACF and PACF to get preliminary understanding of the process
par(mfrow = c(1, 2))
acf(milestimeseries)
pacf(milestimeseries)

# Step 3: The suspension bridge pattern in ACF suggests both nonstationarity
# and strong seasonality.  Perform a non-seasonal difference to give an 
#ARIMA(0,1,0) model

par(mfrow = c(1, 1))
milestimeseriesdiff1 <- diff(milestimeseries, differences = 1)
milestimeseriesdiff1
plot(milestimeseriesdiff1)

# Step 4: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 4))
acf(milestimeseriesdiff1)
pacf(milestimeseriesdiff1)

# Step 5: The differenced series looks stationary but has strong seasonal lags
# Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)
par(mfrow = c(1, 1))
milestimeseriesseasonaldiff1 <- diff(milestimeseries, lag = 12, 
                                     differences=1)
milestimeseriesseasonaldiff1
plot(milestimeseriesseasonaldiff1)

# Step 6: Check ACF and PACF for seasonally differenced data
#to explore remaining dependencies
par(mfrow = c(2, 4))
acf(milestimeseriesseasonaldiff1)
pacf(milestimeseriesseasonaldiff1)

# Step 7: Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Perform a non-seasonal differencing.
# ARIMA(0,1,0)(0,1,0)12
par(mfrow = c(1, 1))
milestimeseriesSeasNoSeasdiff1 <- 
  diff(milestimeseriesseasonaldiff1, differences=1)
milestimeseriesSeasNoSeasdiff1
plot(milestimeseriesSeasNoSeasdiff1)

# Step 8: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesSeasNoSeasdiff1)
pacf(milestimeseriesSeasNoSeasdiff1)

# Step 9: ACF and PACF show significant lag-1, which then cutoff, requiring
# an AR(1) and an MA(1) term.  Also, the significant lag at the seasonal
# period is negative, requiring a SeasonalMA(1) term
milesArima1 <- Arima(milestimeseries, order = c(1,1,1),
                    seasonal = c(0,1,1), include.drift = FALSE)
milesArima1
auto.arima(milestimeseries)

# Step 10: Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(milesArima1$residuals, lag.max = 24)
mean(abs(milesArima1$residuals))

pacf(milesArima1$residuals, lag.max = 24)
#Box.test(milesArima1$residuals, lag=24, type="Ljung-Box")

# Step 11: Start forecasting
par(mfrow = c(1, 1))
#milestimeseriesforecastsArima1 <- forecast.Arima(milesArima1, h=36)
milestimeseriesforecastsArima1 <- forecast(milesArima1, h=36)
#plot.forecast(milestimeseriesforecastsArima1)
plot(milestimeseriesforecastsArima1)
milestimeseriesforecastsArima1

#ARIMA - Model 2
#Step-by-step ARIMA model building
# Model 2
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
miles = read.csv("us-air-carrier-traffic.csv")
miles
milestimeseries <- ts(miles, frequency = 12, start = c(1996,1))
milestimeseries
par(mfrow = c(1, 1))
plot(milestimeseries)

# Step 2: Perform a seasonal differencing on the original time series (ARIMA(0,0,0)(0,1,0)12)
par(mfrow = c(1, 1))
milestimeseriesseasonaldiff1 <- 
  diff(milestimeseries, lag = 12, differences=1)
milestimeseriesseasonaldiff1
plot(milestimeseriesseasonaldiff1)

# Step 3: Check ACF and PACF for seasonally differenced data
#to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milestimeseriesseasonaldiff1)
pacf(milestimeseriesseasonaldiff1)

# Step 4: Strong positive autocorrelation indicates need for either an AR component
# or a non-seasonal differencing.  Add an AR term.
# ARIMA(1,0,0)(0,1,0)12
milesArima2 <- Arima(milestimeseries, order = c(1,0,0),
                     seasonal = c(0,1,0), include.drift = TRUE)
milesArima2

# Step 5: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milesArima2$residuals)
pacf(milesArima2$residuals)
Box.test(milesArima2$residuals, lag=24, type="Ljung-Box")

# Step 6: Strong negative autocorrelation at the seasonal period
#indicates need for a seasonal MA term. ARIMA(1,0,0)(0,1,1)12
milesArima2 <- Arima(milestimeseries, order = c(1,0,0),
                     seasonal = c(0,1,1), include.drift = TRUE)
milesArima2

# Step 7: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
acf(milesArima2$residuals)
pacf(milesArima2$residuals)
Box.test(milesArima2$residuals, lag=24, type="Ljung-Box")

# Step 8: Start forecasting
par(mfrow = c(1, 1))
#milestimeseriesforecastsArima2 <- forecast.Arima(milesArima2, h=36)
milestimeseriesforecastsArima2 <- forecast(milesArima2, h=36)
#plot.forecast(milestimeseriesforecastsArima2)
plot(milestimeseriesforecastsArima2)
milestimeseriesforecastsArima2

# Automated functions are available
milesAutoArima <- auto.arima(milestimeseries,ic='aic')
milesAutoArima
#milestimeseriesforecastsAutoArima <- forecast.Arima(milesAutoArima, h=36)
milestimeseriesforecastsAutoArima <- forecast(milesAutoArima, h=36)
#plot.forecast(milestimeseriesforecastsAutoArima)
plot(milestimeseriesforecastsAutoArima)
milestimeseriesforecastsAutoArima

