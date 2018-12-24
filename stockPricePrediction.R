library("forecast")
library("stats")
library("data.table")
library("lubridate")

setwd("C:/Users/SahilMattoo/Desktop/Udemy/TimeSeries/GLAKES/StockPreds")
data = read.csv("apple.csv", stringsAsFactors = F)
data$date = as.Date(data$date, "%Y-%m-%d")
head(data)
tail(data)

## Insert week days into the data
data$day = weekdays(data$date)

## Insert week number into the data
data$week = week(data$date)
#month(data$date)

## Insert month number into the data
data$month = month(data$date)

## Insert year into the data
data$year = year(data$date)

##### weekly data set #####

data.week = data.frame()
weekNo = unique(data$week)
#class(weekNo)#numeric
for(week_i in weekNo){
  subData = subset(data, data$week == week_i)
  subData = subData[!duplicated(subData$year),]
  if(nrow(data.week) == 0){
    data.week = subData
  } else {
    data.week  = rbind(data.week, subData)
  }  
}
## Observe the order of the data
## Lets change the order of the days
data.week = data.week[order(data.week$date),]
data.week.train = subset(data.week, data.week$date <= as.Date("2017-12-31", "%Y-%m-%d"))
data.week.test = subset(data.week, data.week$date > as.Date("2017-12-31", "%Y-%m-%d"))

priceTrain.ts = ts(data.week.train$close, start = c(2007,1), frequency = 52)
priceTest.ts = ts(data.week.train$close, start = c(2018,1), frequency = 52)
## Auto ARIMA
model_arima = auto.arima(priceTrain.ts)
## Random ARIMA model
accuracy(model_arima)
summary(model_arima)

## Random ARIMA model
model_arima = Arima(priceTrain.ts, order = c(0,1,1), include.drift = TRUE)
accuracy(model_arima)
summary(model_arima)

model_hw = HoltWinters(priceTrain.ts)
summary(model_hw)

fit_autoarima <- forecast(auto.arima(priceTrain.ts), h = nrow(data.week.test))
fit_hw <- forecast(HoltWinters(priceTrain.ts), h = nrow(data.week.test))

class(fit_autoarima)
ff = fit_autoarima
class(ff)
ff=as.data.frame(ff)
# COMPARE FORECAST ACCURACIES ACROSS DIFFERENT METHODS USED
?accuracy.default
round(accuracy(fit_autoarima, data.week.test$close), 3)
round(accuracy(fit_hw, data.week.test$close), 3)

#####
## Excercise1: Improve the model accuray. Build model manually

## Excercise2: Predict monthly prices

## Excercise3: Do similar predictions for GE, Google and GS 