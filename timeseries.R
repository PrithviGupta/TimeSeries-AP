#Time-Series 
data(AirPassengers) 
AirPassengers 
str(AirPassengers) 
class(AirPassengers) 
#Check for missing values 
sum(is.na(AirPassengers)) 

start(AirPassengers) 
end(AirPassengers) 

frequency(AirPassengers) 

summary(AirPassengers) 
par(mfrow=c(3,3)) 
plot(AirPassengers) 

plot.ts(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers))) #This will print the cycle across years 
cycle(AirPassengers)

#Step 3: Make it stationary

plot(log(AirPassengers))
plot(diff(log(AirPassengers)))

plot(aggregate(AirPassengers,FUN=mean))

boxplot(AirPassengers~cycle(AirPassengers))

plot(diff(log(AirPassengers))) 

#Step 5: Model Identification and Estimation

acf(AirPassengers) 
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
plot(diff(log(AirPassengers)))

#Step 6: ARIMA Model Prediction

fit <- arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
fit
pred <- predict(fit,n.ahead=10*12) 
pred1<-round(2.718^pred$pred,0)
pred1 

ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))

data1<-head(pred1,12) 
data1
predicted_1960 <- round(data1)
original_1960 <- tail(AirPassengers,12) 

datawide <- ts(AirPassengers, frequency = 12, start=c(1949,1), end=c(1959,12)) 
datawide
fit1 <- arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period=12)) 
pred <- predict(fit1,n.ahead=10*12) 
pred1<-2.718^pred$pred 
pred1 

data11=round(head(pred1,12),0) 
data22=round(tail(AirPassengers,12),0) 

plot(data11,col="red", type="l") 
lines(data22,col="blue")
#Step 7: Check normality using Q-Q plot

qqnorm(residuals(fit)) 
qqline(residuals(fit))

