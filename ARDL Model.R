####### LOADING LIBRARIES

library(readxl)
library(tseries)
library(astsa)
library(FitARMA)
library(forecast)
library(sandwich)
library(rugarch)
library(lubridate)
library(data.table)
library(zoo)
library(tframePlus)
library(stats)
library(timeSeries)

####### READING DATA

setwd("C:/Users/Scott Padgett/OneDrive - University of Georgia/MSBA/2) Time Series")

gdppot <- read_excel(skip=10,"GDPPOT.xls")
gdpdef <- read_excel(skip=10,"GDPDEF.xls")
gdpc1 <- read_excel(skip=10,"GDPC1.xls")
fedfunds <- read_excel(skip=10,"FEDFUNDS.xls")


attach(fedfunds) # fed funds rate
ts_fedfunds <- ts(data = FEDFUNDS, frequency = 12, start = c( 1954, 07), end = c(2019,02))
attach(gdpc1) # real gdp
ts_gdpc1 <- ts(data = GDPC1, frequency = 4, start = c( 1947, 01), end = c(2018,10))
attach(gdpdef) # gdp deflator
ts_gdpdef <- ts(data = GDPDEF, frequency = 4, start = c( 1947, 01), end = c(2018,10))
attach(gdppot)  # potential gdp
ts_gdppot <- ts(data = GDPPOT, frequency = 4, start = c( 1949, 01), end = c(2029,10))


####### TRANSFORMATIONS AND CALCULATIONS

## fed funds to quarterly
quar_ts_fedfunds <- as.quarterly(ts_fedfunds, FUN = mean)

## calculating inflation rate
ln_deflat = log(ts_gdpdef)
inflation = 400*(ln_deflat - stats::lag(ln_deflat,-1))   ## minus 3 or 1?


## calculating output GDP
ln_real_gdp = log(ts_gdpc1)
ln_pot_gdp = log(ts_gdppot)

output_gdp = 100*(ln_real_gdp - ln_pot_gdp)

print(output_gdp)

## sample creation
start1 <-  c(1980,1)
end1 <- c(2018,04)
inflation_smpl <- window(inflation, start = start1, end = end1)
output_gdp_smpl <- window(output_gdp, start = start1, end = end1)
rate_smpl = window(quar_ts_fedfunds, start = start1, end = end1)
                          
## ARDL Model

ARDL <- lm(rate_smpl ~ inflation_smpl + output_gdp_smpl)
summary(ARDL)


##### A)

mean(rate_smpl)
sd(inflation_smpl)

# Mean: 4.73406
# SD: 1.778921

##### B) 

# With a coefficient estimate of 1.7501 and an error of .1180, inflation is shown to be statistically 
# signifigant and can be interpereted as such, "A 1%  increase in inflation will increase
# the federal funds rate by 1.705%.

# With a coefficient estimate of .2598  and a standard error of .1068, output gdp is shown
#  to be statistically signifigant and can be interpereted as, " A 1 billion dollar increase
# in the GDP output gap increases inflation by .25 %"

##### C)

# by increasing the quarterly federal funds rate by 1.779 * 1.7501 = 3.1134%

##### D) 

rateLag <- stats::lag(rate_smpl, -1)
print(rateLag)
print(rate_smpl)

ARDL_lag <- lm(rate_smpl ~ stats::lag(rate_smpl, -1) + inflation_smpl + output_gdp_smpl)
summary(ARDL_lag)


# The inclusion of a lag term completely changes the coeffieients of both inflation and output
# gap. inflation retains its signifigance with a coefficient estimate of -4.67e-16 and std. error of 
# 1.118e-16 and the output gap loses its signifigance with a coefficient estimate of 
# -2.812e-17 and a standard error of 6.604e-17. Both of these values have a much smaller and 
# negative effect on the federal funds rate.








