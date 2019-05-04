# Time series regression
# Replicates Fama's test of interest rates as predictors of inflation

library(readxl); library(tseries); library(astsa); library(FitARMA); library(forecast)
library(sandwich) # allows computation of robust standard errors

# Read in and set up data
data1 = read_excel(skip=10, "TB3MS.xls")
data1
attach(data1)
rate = ts(data=TB3MS, frequency = 12, start = c(1934,1), end = c(2019,2))
data2 = read_excel(skip=10, "CPIAUCSL.xls"); attach(data2)
cpi = ts(data=CPIAUCSL, frequency = 12, start = c(1947,1), end = c(2019,1))
lncpi = log(cpi)
inflation = 400.*(lncpi - lag(lncpi,-3))
variables = ts.union(rate,inflation)

# Define estimation sample
start1 = start = c(1950,1); end1 = end = c(2018,12)
sample = window(variables, start = start1, end = end1)
inflation_smpl = window(inflation, start = start1, end = end1)
rate_smpl = window(lag(rate,-3), start = start1, end = end1)
plot.ts(sample)
ts.plot(sample)

# Run regression and compute robust standard errors using Newey-West
regression1 = lm(inflation~lag(rate,-3), data = sample); summary(regression1)
omega_nw = NeweyWest(regression1,lag = 4); omega_nw
se_constant = sqrt(omega_nw[1,1]); se_constant
se_beta = sqrt(omega_nw[2,2]); se_beta

# Alternative function to get robust standard errors
vcv1_hvac = vcovHAC(regression1); vcv1_hvac # heteroskedasticity and auto correlation robust
vcv1_hv = vcovHC(regression1); vcv1_hv # heteroskedasticity robust only

# Test Fama's restriction using NeweyWest standard errors
b1 = summary(regression1)$coefficients[2,1]; b1
tstat = (b1-1)/sqrt(omega_nw[2,2]); tstat
 
# Forecast inflation using ARDL
# Forecast lagged interest rate
rate_arima = Arima(rate_smpl, order = c(1,0,0)) # arima model for interest rate
rate_forecast = forecast(rate_arima) # forecast interest rate
rate_xvalues = summary(rate_forecast) # store forecast values
# Now estimate ARDL(1,0) and use rate forecasts in forecast function 
ardl1 = Arima(inflation_smpl, order = c(1,0,0), xreg = rate_smpl) # arima model with exogenous regressors (ARDL)
summary(ardl1)
ardl1_infl_forecast = forecast(ardl1, xreg=rate_xvalues[,1]) # forecast using forecasted rates from first model 
summary(ardl1_infl_forecast); plot(ardl1_infl_forecast)
ardl2 = Arima(inflation_smpl, order = c(1,0,0)) # arima model for comparison
forecast(ardl2)

as.Data
