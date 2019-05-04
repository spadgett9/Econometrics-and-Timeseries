library(readxl)
library(astsa)
library(tseries)


#################################################################################################


setwd("C:/Users/Scott Padgett/OneDrive - University of Georgia/Random")


unrate <- read_xls("UNRATE.xls")


#################################################################################################


head(unrate, 10)

tsplot(unrate)      # part of astsa

unrate_ts = ts(unrate$UNRATE, frequency = 12, start =c(1948,1), end =c(2017,11))  # need to do this to define as tseries

plot.ts(unrate_ts)   # looks similar to above, but as time series instead of batch

################################################################################################

cpi = read_xls("CPIAUCSL.xls")

cpi_ts = ts(cpi$CPIAUCSL, frequency = 12, start = c(1947,1), end = c(2017,11))

trend = seq_along(cpi_ts)        # making a trend line

trend

trend_reg <- lm(cpi_ts ~ trend)  # running the regression

summary(trend_reg)

cpi_dt = cpi_ts - cpi_trend      # getting the error term

cpi_tplot = ts.union(cpi_ts,cpi_trend,cpi_dt)

plot(cpi_tplot)



summary(trend2_reg <- lm(cpi_ts ~ time(cpi_ts)))   # Another way to regress. dont know diff. something about time function

tsplot(cpi_ts)
abline(trend2_reg)


# taking the log to make plot linear

ln_cpi <- log(cpi_ts)
trend_reg3 <- lm(ln_cpi ~ time(ln_cpi))
tsplot(ln_cpi)
abline(trend_reg3)
inflation = diff(ln_cpi)*1200  #growth rate is difference of the log (% growth). 12 = annual, 100 = reverse percentage

ts.plot(inflation, unrate_ts)

#althernative methods: Subsampling

vars = ts.union(unrate_ts, inflation)
plot(vars)
vars_ss =window(vars, start=c(1960,1), end=c(2008,12))
plot(vars_ss)

# lagging for first differencing

inf2 = ln_cpi - lag(ln_cpi, -1)  # first differencing inflation by one period. (origional plot - plot adjusted back on year)

lncpi_lag = lag(ln_cpi, -1)     # now the same with the cpi

vars2 = ts.union(ln_cpi, lncpi_lag)

vars2






##########################################################################################################################

                                                     # cLASS 1/23 #

##########################################################################################################################



library(readxl)
library(tseries)
library(astsa)
library(ggplot2)


set.seed(503)    # allows to draw random values from same pool (pool 503) of random values

# here were going to simulate a time series, with white noise in a random walk

epsilon = rnorm(150,0,1)  # standard normal random draw with 150 draws

y1  = cumsum(epsilon) # start at time zero then add the white noise to each previous draw (keep summing these together)

epd = epsilon + .25; y2 = cumsum(epd)  # random walk with drift of .25 (a constant which just shifts the graph a bit)

# Graph of random walk with and without drift and just white noise

tsplot(y2, ylim=c(-10,42))  
lines(y1, col=4)
lines(epsilon, col=3)

# auto correlations

acf1(y1)
acf2(y2, partial = FALSE)
acf1(epsilon)




library(help = "stats")









