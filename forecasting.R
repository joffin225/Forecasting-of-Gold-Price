#Importing necessary libaries
library(forecast)
library(tseries)

#Reading data from CSV
gold=read.table(file.choose(),header=T,sep=",")
head(gold)

#Converting to time series formate
gold_price <- ts(gold$Price,start = c(1991,1), frequency =12)
class(gold_price)
View(gold_price)
frequency(gold_price)
summary(gold_price)

#Examining the Data
plot(gold_price)
adf.test(gold_price, alternative = "stationary") 
acf(gold_price)
pacf(gold_price)

library(ggfortify)
decomposeAP <- decompose(log(gold_price),"additive")
autoplot(decomposeAP)

plot(gold_price)
abline(reg=lm(gold_price~time(gold_price)))

#Differencing once
diff1<- diff(gold_price)
plot(diff1)
adf.test(diff1)
acf(diff1,main="Differencing once")
pacf(diff1,main="Differencing once")

#Differencing twice
diff2<- diff(gold_price,differences = 2)
plot(diff2)
adf.test(diff2)
acf(diff2,main="Differencing twice")
pacf(diff2,main="Differencing twice")

#Natural log transformation and differenced once
ln=log(gold_price)
diffln=diff(ln)
plot(diffln)
adf.test(diff(log(gold_price)),k=0)
acf(diffln,main="ln Transformation & Differencing once")
pacf(diff1,main="ln Transformation & Differencing once")

#Natural log transformation and differenced twice
lnGold=log(gold_price)
difflnGold=diff(lnGold,differences = 2)
plot(difflnGold)
adf.test(difflnGold)
acf(difflnGold,main="ln Transformation & Differencing once")
pacf(difflnGold,main="ln Transformation & Differencing once")

cat('Data is stationary at log transformation and differencing once')

#Fiting ARIMA model
arimaFit=auto.arima(lnGold)

#Comparing with mannual fitted model
fit <- arima(log(gold_price), c(0, 1, 1),seasonal = list(order = c(0,1,1),period = 12))
summary(fit)


#Forecast using model
pred <- predict(fit, n.ahead =4)
pred1 = 2.718^pred$pred
ts.plot(gold_price,2.718^pred$pred,log = "y",lty=c(1,3))

# DIAGNOSTIC CHECKING
ggtsdiag(fit)

arima <- forecast(fit, h=5)
accuracy(arima)

#Plotting the forecast
plot(arima)
abline(h=5)


# TESTING THE MODEL

datawide = ts(gold_price,frequency =12,start=c(1991,1),end= c(2013,12))
fit = arima(log(datawide),c(0,1,1),seasonal = list(order = c(0,1,1),period=12))
pred = predict(fit,n.ahead = 4*12)
pred1 = 2.718^pred$pred
data1 = head(pred1,16)

predicted_2014 = round(data1,digits=0)

original_2014 = tail(gold_price,16)

ts.plot(gold_price,2.718^pred$pred,log = "y",lty=c(1,3))

