library(astsa)
library(tseries)
library(forecast)
#library(xts)
#library(TSA)

# Data Exploration
data <- read.csv("SPY_5Y.csv",TRUE,",")
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
head(data)

# Data Cleaning
colSums(is.na(data))
anyDuplicated(data$Day)
summary(data)

#to time series
ts_data1 <- xts(data$Close, order.by = data$Date)
plot(ts_data1)

ts_data = ts(data$Close, frequency = 1)


# Checking Stationary
adf.test(ts_data)# not stationary

acf2(ts_data) # AR model

# Train Test Set

split <- round(length(ts_data) * .75)

#split_data <- as.Date("2023-05-21")
#train1 <- ts_data1[index(ts_data) <= split_data]
#test1 <- ts_data1[index(ts_data) > split_data]
#plot(train1)

train <- ts(ts_data[1:split])
test <- ts(ts_data[(split + 1) : length(ts_data)],start = (split + 1))

write.csv(ts_data, "TimeSeries_Clean.csv")
write.csv(train, "Train.csv")
write.csv(test, "Test.csv")

#Decomposed time series
seasonal <- ts(train, frequency = 12)
decomp <- decompose(seasonal)
plot(decomp)

seasonal
# Auto Correlation Function
acf2(seasonal)

#Spectral Analysis
spectrum(seasonal)

#Prediction

#my Model 1
sarima_model <- Arima(train, 
                      order = c(1, 0, 0),            
                      seasonal = list(order = c(0, 1, 0), period = 12))
summary(sarima_model)
checkresiduals(sarima_model)
sarima(train1, 1, 0, 0 , 0, 1 ,0, 12)
#my model 2
sarima_model2 <- Arima(train, 
                      order = c(1, 0, 0),            
                      seasonal = list(order = c(0, 1, 1), period = 12))
summary(sarima_model2)
checkresiduals(sarima_model2)
sarima(train, 1, 0, 0 , 0, 1 ,1, 12)

#Model 1
sarima.for(train, n.ahead = length(test),plot.all = TRUE, p = 1,d = 0, q =0, P = 0, D = 1, Q = 0, S = 12)
lines(test, col = "blue")
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))
#Model 2
sarima.for(train, n.ahead = length(test),plot.all = TRUE, p = 1,d = 0, q =0, P = 0, D = 1, Q = 1, S = 12)
lines(test, col = "blue")
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))

# Future Model 1
sarima.for(ts_data, n.ahead = 365,plot.all = TRUE, p = 1,d = 0, q =0, P = 0, D = 1, Q = 0, S = 12)
lines(test1, col = "blue")
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))

#Future Model 2
sarima.for(ts_data, n.ahead = 365 ,plot.all = TRUE, p = 1,d = 0, q =0, P = 0, D = 1, Q = 1, S = 12)
legend("topleft", legend = c("Forecast","Test","Training"), 	col=c("red", 	"blue","black"), lty = c(1,1))                                                                                                      


