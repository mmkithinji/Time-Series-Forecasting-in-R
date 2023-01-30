### This script consists of analysis of NSE 20 share index data using ARIMA model with (G)ARCH volatility
### The script is written by Martin Kithinji, mkithinji@kyu.ac.ke
### The data consists of NSE 20 share index from 3RD JAN 2006 - 31ST MARCH 2021


###Loading required packages. Note that any missing package should be installed via the TOOLS menu or console window
library(ecdfHT)
library(tseries)
library(forecast)
library(moments)
library(VGAM)
library(aTSA)
library(FinTS)
library(fGarch)
library(rugarch)
### Importing the excel data. Note that this is replaced with the file path in your device before running this pice of code 
data1<-read.csv("C:\\Pau\\Research\\Risk Measures\\CODE\\KenyaNSE20HistoricalData.csv",header = T)
###Price data analysis
pr<-gsub(",", "", data1[,2])  #to replace all seperators to ensure comma separation of entires 
pr<-as.numeric(pr) ## defining the object "pr" and assigning it price in form of numeric data

###We first plot the data to identify any desirable features of the time series
plot(pr, type = "l", xlab = "t", ylab = expression(X[t]), main = "NSE 20 share index price path")

##unit root tests for testing stationarity
tseries::adf.test(pr)
tseries::pp.test(pr)
tseries::kpss.test(pr)

####Confirm even from the ACF and PACF plot that the price data is not stationary
par(mfrow=c(1,2))
acf(pr, main="NSE 20 price ACF")
pacf(pr, main="NSE 20 price PACF")
par(mfrow=c(1,1))


### IF the data is not stationary check the number of differences required to achieve stationarity
ndiffs(pr)

##Difference the data and analyze the stationary differenced data, otherwise use mathematically traceable transformations to achieve stationarity
pr1<-diff(pr,differences = 1)

##Plot both the differenced and undifferenced data and observe the difference in the two plots
par(mfrow=c(1,2))
plot(pr, type = "l", xlab = "t", ylab = expression(X[t]), main = "NSE 20 share index price path")
plot(pr1, type = "l", xlab = "t", ylab = expression(X[t]), main = "Differenced NSE 20 share index price path")
par(mfrow=c(1,1))

###Now use the ACF and PACF to obtain an initial guess of the order of the ARIMA Model
par(mfrow=c(1,2))
acf(pr1, main="Differenced NSE 20 price ACF")
pacf(pr1, main="Differenced NSE 20 price PACF")
par(mfrow=c(1,1))

###Autofit ARIMA for price data
fit.arima<-auto.arima(pr)
fits.arima<-auto.arima(pr1)
###Manual fit for price data to confirm the model that has the lowest AIC,AICC and BIC
AR.P<-3
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
## ARIMA (2,0,2) fit has the lowest AIC,AICC and BIC which confirms the auto.arima output
##We then forecast using both the stationary and non-stationary fits.
forecast.fit=predict(fit.arima, 252)
forecast.fits=predict(fits.arima, 252)
ts.plot(ts(pr),round(forecast.fit$pred,2), lty = c(1,3), col=c(5,2), ylab = "NSE 20 share price")
ts.plot(ts(pr1),round(forecast.fits$pred,2), lty = c(1,3), col=c(5,2), ylab = "NSE 20 share price")
#####The second approach involves transforming the price data into returns. This is because returns are mostly stationary
#Returns data analysis.
NegReturn<-as.matrix(data1[,10])
X<-as.numeric(NegReturn[2:3788]) ##Note that this is calculated from the data
X<-na.omit(X)
par(mfrow=c(2,2))
plot(X, type="l",xlab = "t", ylab = expression(X[t]), main="NSE 20 share index loss return")
plot(pr1, type = "l", xlab = "t", ylab = expression(X[t]), main = "Differenced NSE 20 share index price path")
plot(pr, type = "l", xlab = "t", ylab = expression(X[t]), main = "Non-Differenced NSE 20 share index price path")
par(mfrow=c(1,1))
tseries::adf.test(X)
par(mfrow=c(1,2))
acf(X, main="NSE 20 returns ACF",lag.max = 20)
pacf(X, main="NSE 20 returns PACF",lag.max = 20)
par(mfrow=c(1,1))
summary(X)
sd(X)
skewness(X)
kurtosis(X)
hist(X, probability = T)
lines(density(X),col="red")
X<-as.vector(X)
ecdfHT(X , type="l")
title("cdf plot of returns")


###random normal simulation
rdata<-rnorm(1000, mean = 0, sd=1)
skewness(rdata)
kurtosis(rdata)
hist(rdata,probability = T)
lines(density(rdata), col="red")
plot(rdata, type="l")
ecdfHT( rdata , type="l")
title("rdata cdf plot")

###Autofit ARIMA for returns
auto.arima(X) ###This is from the forecast package

###Note that auto.arima selects best model based on stability of forecasts but
###the choice is sometimes not parsimonous, hence we sometimes result to manual fits 
### and select a model based on information criterion.

###Manual fit for returns to confirm Automatic fits
orr<-3
orc<-3
######
arima.sumry<-data.frame()
for (i in 1:orr) {
  for (j in 1:orc) {
    order1<-c(i,0,j)
  fit.t1<-Arima(X, c(i,0,j))
  eval.t<-data.frame(fit.t1$aic,fit.t1$aicc,fit.t1$bic,i,0,j)
  arima.sumry<-rbind(arima.sumry,eval.t)
  }
}
arima.sumry
##chosen model ARIMA(2,1,2)
###Modeling volatility
fit.t<-Arima(X,c(2,0,2))
###Extact and plot the residuals to identify any desirable features
er<-fit.t$residuals
plot(er)
##Use the ARCH Test to confirm prescence of heteroscedatsicity in the errors
FinTS::ArchTest(er^2, lags=12) ###In our case, there is heteroscedasticity
#p-value<alpha hence we reject the null hypothesis and conclude that there is presence of ARCH effects

##Similarly use the ACF and PACF plots to identify the number of significant lags
par(mfrow=c(1,2))
acf(er^2, main="ACF of squared residuals")
pacf(er^2, main="PACF of squared residuals")
par(mfrow=c(1,1))

###ARCH fit using fGarch package
###Note that you can fit both ARMA and ARCH/GARCH concurently, however we chose to fit an
###ARIMA followed by ARCH/GARCH using fGarch package to enhance our understanding

###ARCH fit using fGarch package
###You are required to alter the order of the ARCH around suggestions from your ACF 
###and PACF plots and select the best model based on information criterion

arch.fit <- garchFit(~garch(1,0), data = er)
summary(arch.fit)

###You are required to alter the order of the GARCH around suggestions from your ACF 
###and PACF plots and select the best model based on information criterion

garch.fit = garchFit(~ garch(1, 1), data = er)
summary(garch.fit)

###concurrent fit of an ARMA and GARCH using rugarch package. This is the right way
###You are required to alter the order of the GARCH around suggestions from your ACF 
###and PACF plots and select the best model based on information criterion


or.spec <- rugarch::ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL,
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   
                   mean.model     = list(armaOrder = c(2, 2), 
                                         external.regressors = NULL),
                                         start.pars = list(), 
                                         fixed.pars = list(),
                   distribution.model = "std")

garch.fit <- rugarch::ugarchfit(spec = or.spec, data = X, solver.control = list(trace=0))
garch.fit

###Remember to also change the distribution of the standardised residuals and observe
### the changes in the information criterion values

