# Unit root tests for natural log of industrial production and inflation

library(readxl)
library(tseries)
library(urca)
data1 = read_excel("INDPRO.xls")
attach(data1)
ip = ts(data=INDPRO,frequency=12, start=c(1919,1), end=c(2018,12))
data2 = read_excel("CPIAUCSL.xls"); attach(data2)
cpi = ts(data=CPIAUCSL, frequency = 12, start = c(1947,1), end = c(2017,11))
start1 = start = c(1950,1); end1 = end = c(2017,11) # set sample period
kstep = 6 # sets order of AR

lnip = log(ip); dlnip = diff(lnip)
lncpi = log(ip); inflation = diff(lncpi)

# Log industrial production: regression model AR(k) with constant and trend 
acf( window(lnip, start = start1, end = end1) ) # autocorrelation function
adf.test(window(lnip, start = start1, end = end1), k = kstep) # augmented Dickey-Fuller test for unit root (for AR(k) process)
pp.test(window(lnip, start = start1, end = end1)) # Phillips-Perron test for unit root (no need to choose lag length)

# Growth rate of industrial production
acf( window(dlnip, start = start1, end = end1))
adf.test(window(dlnip, start = start1, end = end1), k = kstep)
pp.test(window(dlnip, start = start1, end = end1))

# Price level
acf( window(lncpi, start = start1, end = end1))
adf.test(window(lncpi, start = start1, end = end1), k = kstep)
pp.test(window(lncpi, start = start1, end = end1))

# Inflation
acf( window(inflation, start = start1, end = end1))
adf.test(window(inflation, start = start1, end = end1), k = kstep)
pp.test(window(inflation, start = start1, end = end1))









