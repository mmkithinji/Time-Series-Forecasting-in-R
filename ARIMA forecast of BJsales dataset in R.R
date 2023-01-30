data()    #To list the data sets in R
#View(BJsales) #To display the BJsales data set in source code editor window

class(BJsales) # To check the type of data set

# Check structure of the data
str(BJsales)

length(BJsales)
head(BJsales)
tail(BJsales)

plot(BJsales) # To plot the data for visual inspection
## From the plot we observe some bullish trend in the data


library(moments)
summary(BJsales)


#Checking symmetry and tails of the distribution 
skewness(BJsales)# Skewness > 0 meaning distribution is not symmetrical
kurtosis(BJsales)# Kurtosis < 3 meaning tails that the distribution is light-tailed


par(mfrow=c(1,2)) #dividing plot area into 2 columns
plot(BJsales) # clear upward trend seen
hist(BJsales, probability = T)
lines(density(BJsales),col='red') #distribution is not symmetrical


par(mfrow=c(1,1))
library(ecdfHT)
X<-as.vector(BJsales) #converting the TS data set into a vector because the input of ecfHT() should be a vector
ecdfHT(X , type="l")
title("empirical cdf plot of data")

###This ends EDA, we now start creating a time series model for the data set


#Checking for autocorrelation in dataset
par(mfrow=c(1,2))
acf(BJsales)
pacf(BJsales)# No signicant lags in PACF but ACF doesn't decay exponentially to zero, implying that data is not stationary

#Check for trend in the data set
par(mfrow=c(1,1)) 
plot(BJsales)
Time<-1:150
data<-as.data.frame(BJsales)
data1<-cbind(Time,data)
str(data1)
lm(BJsales~Time, data1)
abline(lm(BJsales~Time, data1)) # Visible clear linear trend observed

##unit root tests for testing stationarity
library(tseries)
tseries::adf.test(BJsales) #p-value > 0.05, we fail to reject the null hypothesis and conclude that data is not stationary
tseries::pp.test(BJsales) #p-value > 0.05, we fail to reject the null hypothesis and conclude that data is not stationary
tseries::kpss.test(BJsales)#p-value < 0.05, we reject the null hypothesis and conclude that data is not trend stationary

### Since our data is not stationary we check the number of differences required to achieve stationarity
ndiffs(BJsales)

##Difference the data and analyze the stationary differenced data, otherwise use mathematically traceable transformations to achieve stationarity
pr1<-diff(BJsales,differences = ndiffs(BJsales))

##Plot both the differenced and undifferenced data and observe the difference in the two plots
par(mfrow=c(1,2))
plot(BJsales, type = "l", xlab = "t", ylab = expression(X[t]), main = "Sales data path")
plot(pr1, type = "l", xlab = "t", ylab = expression(X[t]), main = "Differenced Sales data path")
par(mfrow=c(1,1))

###Now use the ACF and PACF to obtain an initial guess of the order of the ARIMA Model
par(mfrow=c(1,2))
acf(pr1, main="Differenced BJ Sales ACF")
pacf(pr1, main="Differenced BJ Sales PACF") #ACF decays exponentially to zero with significant ACF and PACF lags
par(mfrow=c(1,1))

###Autofit ARIMA for price data
fit.arima<-auto.arima(BJsales)
fits.arima<-auto.arima(pr1)
###Manual fit for price data to confirm the model that has the lowest AIC,AICC and BIC
AR.P<-6
MA.Q<-3
######
arima1.sumry<-data.frame()
for (i in 1:AR.P) {
  for (j in 1:MA.Q) {
    order11<-c(i,0,j)
    fit.t11<-Arima(pr1, c(i,0,j))
    eval.t1<-data.frame(fit.t11$aic,fit.t11$aicc,fit.t11$bic,i,0,j)
    arima1.sumry<-rbind(arima1.sumry,eval.t1)
  }
}
arima1.sumry
## ARIMA (1,0,1) fit has the lowest AIC,AICC and BIC which confirms the auto.arima output
##We then forecast using both the stationary and non-stationary fits.
forecast.fit=predict(fit.arima, 10)
forecast.fits=predict(fits.arima, 10)
ts.plot(ts(BJsales),round(forecast.fit$pred,2), lty = c(1,3), col=c("black",2), ylab = "BJ Sales")
ts.plot(ts(pr1),round(forecast.fits$pred,2), lty = c(1,3), col=c("black",2), ylab = "BJ Sales")
