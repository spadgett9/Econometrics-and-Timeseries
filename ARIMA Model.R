######### LIBRARIES
library("readxl")
library("forecast")
library("FitARMA")
library("tseries")
library("astsa")
library("vars")
library("stats")

######### FILE 
setwd("C:/Users/Scott Padgett/OneDrive - University of Georgia/MSBA/2) Time Series")
file <- read.csv("UNRATE.csv")
attach(file)
unrate = ts(UNRATE,frequency=12,start=c(1948,1),end=c(2018,12))
start1 = start=c(1950,1)
end1 = end=c(2016,12) 
sample = window(unrate, start = start1, end = end1)


######### A
mean(sample)
sd(sample)

######### B
acf1(sample, 4)
acf2(sample, 4)

# in both the acf and pacf models the bars through lag 4 exceed the confidence bound, and therefore are likely not white noise
# at the 5% level

######### C
model1 = Arima(window(unrate, start = start1, end = end1), order = c(1,1,4), method = c("CSS"), include.constant = TRUE )
summary(model1)
acf2(residuals(model1))
Box.test(residuals(model1), lag=24, type="Ljung")

# With a X-squared = 65.792 and a p-value = 9.307e-06 we can veritably reject the null hypothesis that all autocorrelations of the
# residuals equal to zero. Therefore we are led to suspect that some or all of our residuals are not white noise (and are thus exibiting serial correlation) 
# which is undesireable for forecasting

######## D
auto.arima(unrate)
auto_model1 = Arima(window(unrate, start = start1, end = end1), order = c(4,1,1), seasonal = list(order = c(2,0,2)),method = c("CSS"), include.constant = TRUE)
summary(auto_model1)
acf2(residuals(auto_model1))
Box.test(residuals(auto_model1), lag = 24, type = "Ljung")

# with an X-squared = 25.156 and p-value = 0.07585 we are unable to reject the null that our residuals are exibiting white noise behaviour
# and have thus eliminated serial correlation within the residuals

######## E
forecast_model1 = forecast::forecast(model1)
forecast_values_model1 = summary(forecast_model1)

forecast_auto_model1 = forecast(auto_model1)
forecast_values_autoModel = summary(forecast_auto_model1)


####### F
oos_start = c(2017,1)
oos_end = c(2018,12)

unrate_oos = window(unrate, oos_start, oos_end)
sumsq = 0
for (i in 1:24) 
{ sumsq = sumsq + (unrate_oos[i]-forecast_values_model1[i , 1])^2 } 
rmse_model1 = sqrt( (1/24)*sumsq ) 


## Auto.model

unrate_oos = stats::window(unrate, oos_start, oos_end)
sumsq1 = 0
for (i in 1:24) # loop over post-T sample period
{ sumsq1 = sumsq1 + (unrate_oos[i]-forecast_values_autoModel[i , 1])^2 } # adding up the squared errors
rmse_autoModel = sqrt( (1/24)*sumsq1 ) # compute root mean square error


rmse_model1
rmse_autoModel








