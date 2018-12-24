# FORECASTING THE STOCK MARKET

library(forecast)
library(stats)
library(data.table)
library(ggplot2)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)

source("helper_functions.R")

setwd("D:/courses/BABI/timeSeries/blrMar25_Apr22/")
# LOAD DATA
data = read.csv("ts_data/financeData.csv", stringsAsFactors = F)

# EXPLORATORY ANALYSIS
sp_500 <- data$sp_500

# TESTS FOR STATIONARITY
adf.test(sp_500)
# p-values are relatively high so we should so visual inspection and
# look at ACF and PACF plots to make appropriate transformation 
# for stationarity. 
sp_500 = ts(sp_500, start = c(1995, 1), frequency = 12)
# TIME SERIES PLOT OF S&P
tsSp <- plot_time_series(sp_500, 'S&P 500')

tsSp
ggplotly(tsSp)

# Here we create the training set where we will compare the values for 2015 
sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)

plot_time_series(sp500_training, 'S&P 500 Training Set')
# Remove comment if you wish to publish plot on ploty
# See GitHub repo for more details
# plotly_POST(timeSeriesPlot, filename = "timeSeriesPlot")

# DECOMPOSING TIME SERIES
sp500_stl <- plot_decomp(sp500_training, 'S&P 500')

sp500_stl
ggplotly(sp500_stl)

# DIAGNOSING ACF AND PACF PLOTS
plot_acf_pacf(sp500_training, 'S&P 500')
# TRANSFORMING OUR DATA TO ADJUST FOR NON STATIONARY
sp500_diff <- diff(sp500_training)

tsDiff <- plot_time_series(sp500_diff, 'First Difference')
tsDiff
ggplotly(tsDiff)

# TESTS FOR STATIONARITY FOR DIFFERENCED TIME SERIES OBJECT
adf.test(sp500_diff)

# p-values seems small enough to infer stationarity for the first difference
# Let's begin analysis with visually inspecting ACF and PACF plots

# DIAGNOSING ACF AND PACF PLOTS FOR DIFFERENCED TIME SERIES OBJECT
plot_acf_pacf(sp500_diff, 'First Difference Time Series Object')

# AUTO.ARIMA ESTIMATION
auto.arima(sp500_training)

# From our visual inspection and auto.arima model we will choose an
# ARIMA(0, 1, 1) with drift 

# BUILD MODEL 
fit <- Arima(sp500_training, order = c(0,1,1), include.drift = TRUE)
summary(fit)


# TEST SET THAT WE WILL COMPARE OUR FORECAST AGAINST 
sp500_test <- read.csv("ts_data/financeDataTest.csv", stringsAsFactors = F)
sp500_test <- ts(sp500_test$Adj.Close, start = c(2015, 1), frequency = 12)

# FORECASTING
# METHOD CHOSEN THROUGH BOX JENKINS METHODOLOGY WAS ARIMA(0,1,1) WITH DRIFT
## ARIMA MODEL CHOSEN 
fit_arima <- forecast(fit, h = 36)

# Holt-Winters
fit_hw <- forecast(HoltWinters(sp500_training), h = 36)

# COMPARE FORECAST ACCURACIES ACROSS DIFFERENT METHODS USED
?accuracy.default
round(accuracy(fit_arima, sp500_test), 3)
round(accuracy(fit_hw, sp500_test), 3)

# CONCLUSIONS
# The model with the best diagnostics is our ARIMA Model 
