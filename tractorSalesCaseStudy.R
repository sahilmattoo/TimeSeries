rm(list=ls(all=TRUE))
setwd("D:/courses/BABI/timeSeries/ts_data/")


library("forecast")
library("stats")
library("data.table")

# Manufacturing Case Study
# Step 1: Convert data into Time Series
par(mfrow = c(1, 1))
TractorSales<-read.csv("Tractor-Sales.csv")
TractorSalesTS<-ts(TractorSales[,2],start = c(2003,1),frequency = 12)
TractorSalesTS
plot(TractorSalesTS, xlab="Years", ylab = "Tractor Sales")
TractorSalesComponents <- decompose(TractorSalesTS, type = "multiplicative")
TractorSalesComponents
plot(TractorSalesComponents)

# Step 2: Difference data to make it stationary
ndiffs(TractorSalesTS)
plot(diff(TractorSalesTS),ylab="Differenced Tractor Sales")

# Step 3: As the time series is not stationary on variance, log transform the data
plot(log10(TractorSalesTS),ylab="Log (Tractor Sales)")

# Step 4: Difference log transformed data to check for stationarity
plot(diff(log10(TractorSalesTS)),ylab="Differenced Log (Tractor Sales)")

# Step 5: Plot ACF/PACF to identify candidates for AR, MA or ARMA models
par(mfrow = c(1,2))
acf(ts(diff(log10(TractorSalesTS))),main="ACF Tractor Sales")
pacf(ts(diff(log10(TractorSalesTS))),main="PACF Tractor Sales")

# Step 6: Build ARIMA model
TractorSalesARIMA <- auto.arima(TractorSalesTS)
TractorSalesARIMA
LogTractorSalesARIMA <- auto.arima(log10(TractorSalesTS))
LogTractorSalesARIMA

# Step 8: Check residuals to ensure they are white noise
par(mfrow=c(1,2))
acf(ts(LogTractorSalesARIMA$residuals),main="ACF Residual")
pacf(ts(LogTractorSalesARIMA$residuals),main="PACF Residual")
Box.test(LogTractorSalesARIMA$residuals, lag=24, type="Ljung-Box")

# Step 8: Forecast Tractor Sales
par(mfrow = c(1, 1))
#TractorSalesForecasts <- forecast.Arima(LogTractorSalesARIMA, h=36)
TractorSalesForecasts <- forecast(LogTractorSalesARIMA, h=36)
#plot.forecast(TractorSalesForecasts, shadecols = "oldstyle")
plot(TractorSalesForecasts, shadecols = "oldstyle")
TractorSalesForecasts

# Alternative method
pred <- predict(LogTractorSalesARIMA, n.ahead=36)
pred

plot(TractorSalesTS,type="l",xlim=c(2004,2018),ylim=c(1,1600),
     xlab = "Year",ylab = "Tractor Sales")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+1.96*pred$se),col="red")
lines(10^(pred$pred-1.96*pred$se),col="green")

