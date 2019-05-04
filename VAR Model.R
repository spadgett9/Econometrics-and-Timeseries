##### LIBRARIES

library(readxl)
library(tseries)
library(vars)
library(forecast)
library(astsa)

##### LOAD DATA

tbl_ppiaco <- read_excel(skip = 10, "PPIACO.xls")
tbl_pcepi <- read_excel(skip=10, "PCEPI.xls")

##### PREPARE DATA

## Create ts files

start_ppiaco <- c(1913, 01)
end_ppiaco <- c(2019, 03)
ts_ppiaco <- ts(data = tbl_ppiaco$PPIACO, start_ppiaco, end_ppiaco, frequency = 12)

start_pcepi <- c(1959, 01)
end_pcepi <- c(2019, 01 )
ts_pcepi <- ts(data = tbl_pcepi$PCEPI, start_pcepi, end_pcepi, frequency = 12)

## Log transformation

log_ppiaco <- log(ts_ppiaco)
log_pcepi <- log(ts_pcepi)

## Union

ts_vars <- ts.union(log_ppiaco, log_pcepi)

## Subsetting

start = c(1970, 01)
end = c(2018, 12)
sub_vars <- stats::window(ts_vars, start, end)

##### VARS MODELING

VARselect(sub_vars, type="both") # chosing to use 2 lags based on SC
lag <- 2
steps <- 48
var1 <- VAR(sub_vars, p = lag, type = c("both"))
summary(var1)

## TESTING GRANGER CAUSALITY

causality(var1, cause = "log_pcepi")

# H0 = C12 = 0 accross all lags of C12 ( the uneployment has no effect of industrial production)
causality(var1, cause = "log_ppiaco")

## 4a) answers

# With a p-value of 0.003037 and 1.355e-06 for the Granger Causality of PCEPI causing PPIACO and for PPIACO causing PCEPI
# respectively, we can reject the null hypothesis that the lags of one variable do not granger cause the left-hand side 
# variable.


# ppiaco affect pcepi

irf1 = irf(var1, n.ahead = steps)
print(irf1); plot(irf1)

print(var1)


## 4b) answers

# It seems from the orthogonalized impulse function plot that the orthogonalized error term from the 
# PPIACO index pushes the value of PCEPI upward continuously up to 48 periods. The incerease is sharp for 3 periods
# but levels out after this to a near linear incresing slope.












