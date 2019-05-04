
library(readxl)
library(tseries)
library(astsa)
library(rugarch)
library(lubridate)
library(data.table)

# Weekly data on the Dow Jones Industrials Average
data1 = read_excel("djia_weekly_1-1-45to2-6-18.xlsx")
attach(data1) 
weekly = seq(as.Date("1945-01-02"), as.Date("2018-02-06"), by = "week") # This defines weekly dates but is not used in the program
djia = ts(data=DJIA, frequency = 52, start = start(c(1945,1))) # This doesn't quite map the dates correctly, since some years have 53 weeks
lndjia = log(djia); dlndjia = diff(lndjia) # Weekly stock returns are measured by log differences 

# Use garch function on returns
garch0 = garch(dlndjia, order = c(0,1)) # GARCH(0,1)
summary(garch0)

garch1 = garch(dlndjia, order = c(1,1)) # GARCH(1,1) 
summary(garch1)

# Use rugarch package and functions for more flexibility
# GARCH(1,1) on returns
gmodel2 = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)))
garch2 = ugarchfit(spec = gmodel2, data = dlndjia)
garch2@fit$coef
hfunc = garch2@fit$sigma
ts.plot(hfunc)
ugarchforecast(garch2)

# GARCH(0,4) on returns
gmodel3 = ugarchspec( variance.model = list(garchOrder = c(0,4)), mean.model = list(armaOrder =c(0,0)) )
garch3 = ugarchfit(spec = gmodel3, data = dlndjia)
garch3@fit$coef

# ARMA(1,1)/GARCH(1,1) on returns
gmodel4 = ugarchspec( variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder =c(1,1)) )
garch4 = ugarchfit(spec = gmodel4, data = dlndjia)
garch4@fit$coef
str(garch4)
