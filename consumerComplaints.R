library("forecast")
library("stats")
library("data.table")

data = read.csv("ts_data/consumeComplaints.csv", stringsAsFactors = F)
head(data)
summary(data)
data_ts = ts(data$noComplaints,start = c(2014,10),frequency = 365)
data_ts
plot(data_ts)
data_decompose = decompose(data_ts)
plot(data_decompose)
model_au_ar1 = auto.arima(data_ts)
model_hw = HoltWinters(data_ts)
accuracy(model_au_ar1)
accuracy(model_ts, data_ts)

x = data$noComplaints[1:50]
y = data$noComplaints[11:50]
x1 = data$noComplaints[10:49]
x2 = data$noComplaints[9:48]
x3 = data$noComplaints[8:47]
x4 = data$noComplaints[7:46]
x5 = data$noComplaints[6:45]
x6 = data$noComplaints[5:44]
x7 = data$noComplaints[4:43]
x8 = data$noComplaints[3:42]
x9 = data$noComplaints[2:41]
x10 = data$noComplaints[1:40]
lm(y ~ x1)
lm(y ~ x1+x2)
lm(y ~ x1+x2+x3)
lm(y ~ x1+x2+x3+x4)
lm(y ~ x1+x2+x3+x4+x5)
lm(y ~ x1+x2+x3+x4+x5+x6)
lm(y ~ x1+x2+x3+x4+x5+x6+x7)
lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8)
lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9)
lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)

### Excercise 1:  Predict number of expected consumer complaints
## Excercise 1.1: Split the data into train and test
## Excercise 1.2: Predict number of consumer complaints using HoltWinters method
## Excercise 1.3: Predict number of consumer complaints using auto.arima method
## Excercise 1.4: Predict number of consumer complaints using manually selected arima method

