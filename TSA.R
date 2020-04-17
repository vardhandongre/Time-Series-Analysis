library(forecast);
library(quantmod);
library(xts);
library(tseries);
library(timeSeries);
library(astsa)

# Data
stockname = 'AAPL' # Apple Stock Data
startdate = '2015-01-01'
enddate = '2019-01-01'

# Data downloaded from Yahoo Finance
stockvar = getSymbols(c(stockname), src = 'yahoo', from=startdate, to=enddate, auto.assign = FALSE)
stockvar = na.omit(stockvar)

# Visualizing Data
chartSeries(stockvar, theme='black', name=c(stockname)[1])
barChart(stockvar,theme='white.mono',bar.type='hlc', name=c(stockname)[1])
lineChart(stockvar,line.type='h',TA=NULL)

price = stockvar[,4]

# Decomposing Data
# Into Trend, Seasonal, Residual
stockvar.ts = ts(price, start=2015-01-02, frequency = 120)
stockvar.de = decompose(stockvar.ts)
plot(stockvar.de)

# Taking Log of Data
log_price = log(price)
# Plot of TS (log_price)
plot.ts(log_price)

# ACF
dlogprice = diff(log_price, lag = 1)
dlogprice = dlogprice[!is.na(dlogprice)]
acf(dlogprice, main = "ACF of Apple Stock")

# Checking Variance
#var(stockvar.ts[1:length(stockvar.ts)/2])
#var(stockvar.ts[length(stockvar.ts)/2:length(stockvar.ts)])

#var(stockvar.ts[1:length(log_price)/2])
#var(stockvar.ts[length(log_price)/2:length(log_price)])

