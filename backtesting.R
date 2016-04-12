#Install the libraries we need
library(quantmod)
library(RCurl)


#Download Michael Kapler's âSystematic Investor Toolboxâ, 
#a powerful set of tools used to backtest and evaluate quantitative trading strategies
sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con = gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)

#Create a new environment
data <- new.env()

#Specify the name of the asset and where the csv file is located on your computer. 
#(You can find more ways to load data here.)
tickers<-spl('USDCAD')

file.path<- "D:/R/Backtesting/data/"

#Load and clean the data
for(n in tickers) { data[[n]] = read.xts(paste(file.path, n, '.csv', sep=''), format='%m / %d / %y %H:%M') }
bt.prep(data, align='remove.na')

#Specify the prices and store our models
prices = data$prices
models = list()

#Create our baseline âBuy and Holdâ strategy
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)

#Calculate the indicators we need for our strategy
CCI20<-CCI(prices,20)
RSI3<-RSI(prices,3)
DEMA10<-DEMA(prices,n = 10, v = 1, wilder = FALSE)
DEMA10c<-prices - DEMA10
DEMA10c<-DEMA10c/.0001

#Set our long entry conditions found by our algorithms and optimized by us in the last post
buy.signal<-ifelse(RSI3 < 30 & CCI20 > -290 & CCI20 < -100 & DEMA10c > -40 & DEMA10c < -20,1,NA)

#Create our long model
data$weight[] = NA
data$weight[] = buy.signal
models$long = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#Set our short conditions
sell.signal<-ifelse(DEMA10c > 10 & DEMA10c < 40 & CCI20 > 185 & CCI20 < 325 & RSI3 > 50, -1 ,NA)

#Create our short model
data$weight[] = NA
data$weight[] = sell.signal
models$short = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#Set the long and short conditions for our strategy
long.short.strategy<-iif(RSI3 < 30 & CCI20 > -290 & CCI20 < -100 & DEMA10c > -40 & DEMA10c < -20,1,iif(DEMA10c > 10 & DEMA10c < 40 & CCI20 > 185 & CCI20 < 325 & RSI3 > 50, -1 ,NA))

#Create our long short strategy
data$weight[] = NA
data$weight[] = long.short.strategy
models$longshort = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#Isolate the dates from our validation set 
#(The data not used to train the model or create the rules, our out-of-sample test)
dates = '2020-01-01::2020-02-27'

#View a plot of our trades
bt.stop.strategy.plot(data, models$longshort, dates = dates, layout=T, main = 'Long Short Strategy', plotX = F)

#View the equity curve and performance statistics.
strategy.performance.snapshoot(models, T)

#The stop loss function
stop.loss <- function(weight, price, tstart, tend, pstop) {
  index = tstart : tend
  if(weight > 0)
    price[ index ] < (1 - pstop) * price[ tstart ]
  else
    price[ index ] > (1 + pstop) * price[ tstart ]
}

#Set our maximum loss at a .25% move in price against our trade
Stoploss = .25/100

#Our long short model with a .25% stop loss
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(long.short.strategy), coredata(prices), stop.loss,pstop = Stoploss)
models$stoploss = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#The plot of our trades
bt.stop.strategy.plot(data, models$stoploss, dates = dates, layout=T, main = 'Stop Loss', plotX = F) 

#And how it compares to the original model
strategy.performance.snapshoot(models[c(1,4:5)], T) 

#The take profit function
take.profit<- function(weight, price, tstart, tend, pprofit) {
  index = tstart : tend
  if(weight > 0)
    price[ index ] > (1 + pprofit) * price[ tstart ]
  else
    price[ index ] < (1 - pprofit) * price[ tstart ]
}

#Maintain at 1:1 risk/reward ratio and set our take profit at a .25% change in price
Takeprofit = .25/100

#Our long short model with a .25% take profit
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(long.short.strategy), coredata(prices), take.profit, pprofit = Takeprofit)
models$takeprofit = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

#The plot of our trades
bt.stop.strategy.plot(data, models$takeprofit, dates = dates, layout=T, main = 'Take Profit', plotX = F)

#Compare it to our other models
strategy.performance.snapshoot(models[c(1,4:6)], T) 


#The stop loss and take profit function
stop.loss.take.profit<-function(weight, price, tstart, tend, pstop, pprofit) {
  index = tstart : tend
  if(weight > 0) {
    temp = price[ index ] < (1 - pstop) * price[ tstart ]
    
    # profit target
    temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]
  } else {
    temp = price[ index ] > (1 + pstop) * price[ tstart ]
    
    # profit target
    temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]
  }
  return( temp )
}

#Our long short model with a .25% stop loss and .25% take profit
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(long.short.strategy), coredata(prices), stop.loss.take.profit,pstop = Stoploss, pprofit = Takeprofit)
models$stop.loss.take.profit = bt.run.share(data, clean.signal=T, trade.summary = TRUE)


#The plot of our trades
layout(1:4)
bt.stop.strategy.plot(data, models$longshort, dates = dates, layout=T, main = 'Long Short', plotX = F)
bt.stop.strategy.plot(data, models$stoploss, dates = dates, layout=T, main = 'Long Short .25% SL', plotX = F)
bt.stop.strategy.plot(data, models$takeprofit, dates = dates, layout=T, main = 'Long Short .25% TP', plotX = F)
bt.stop.strategy.plot(data, models$stop.loss.take.profit, dates = dates, layout=T, main = 'Long Short .25% SL, .25% TP', plotX = F)

#Finally comparing all the models we created
strategy.performance.snapshoot(models[c(1,4:7)], T)
