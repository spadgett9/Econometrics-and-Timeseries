#Load in applicable libraries
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(astsa)
library(tframePlus)
library(stats)
library(vars)
library(forecast)


#Bring in the data
UMCSENT <- read_xls('UMCSENT.xls', skip = 10) #Consistent readings begine 01/1978
PROD <- read_xlsx('BusinessProd.xlsx', skip = 9)

#Tidy up the Productivity data as quarters are represented as columns
PROD <- gather(PROD, key = 'Quarter', value = 'Labor_Productivity', 2:5)
PROD$Quarter <- case_when(PROD$Quarter == 'Qtr1' ~ 01,
                          PROD$Quarter == 'Qtr2' ~ 04,
                          PROD$Quarter == 'Qtr3' ~ 07,
                          PROD$Quarter == 'Qtr4' ~ 10)
PROD$day <- 01
PROD <- unite(PROD, col = 'observation_date', c(1,2,4), sep = '-')
PROD$observation_date <- ymd(PROD$observation_date)
PROD <- arrange(PROD, observation_date)

#Create time series objects
UMCSENT_ts <- ts(UMCSENT$UMCSENT, start = c(1952,11), end = c(2019,3), frequency = 12,
                 class = c('ts')) 

UMCSENT_ts_quart <- as.quarterly(UMCSENT_ts, FUN=mean)
PROD_ts <- ts(PROD$Labor_Productivity, start = c(1947,1), end = c(2018,10), frequency = 4, 
              class = c('ts'))




## Log transformation\. PROD already annualized

log_UMCSENT <- log(UMCSENT_ts_quart)
diff_UMCSENT <- diff(log_UMCSENT)
Ann_UMCSENT <- diff_UMCSENT*400

## Union

ts_vars <- ts.union(Ann_UMCSENT, PROD_ts)

## Subsetting

start = c(1978, 01)
end = c(2016, 10)
sub_vars <- stats::window(ts_vars, start, end)


##### VARS MODELING

VARselect(sub_vars, type="both") # chosing to use 2 lags based on SC
lag <- 2
steps <- 16
var1 <- VAR(sub_vars, p = lag, type = c("both"))
summary(var1)

## TESTING GRANGER CAUSALITY

causality(var1, cause = "PROD_ts")

# H0 = C12 = 0 accross all lags of C12 ( the uneployment has no effect of industrial production)
causality(var1, cause = "Ann_UMCSENT")









start <- c(1978,1)
end <- c(2016,10)

UMCSENT_samp <- stats::window(UMCSENT_ts_quart, start, end)
PROD_samp<- stats::window(PROD_ts, start, end)










