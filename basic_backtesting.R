require(quantmod)
require(PerformanceAnalytics)
library(TTR)
# create list of all .csv files in folder
rawlist<-read.csv("D:/R/stock/Auto_Stock/list.csv",header=TRUE,sep=",")
stocklist<-as.vector(rawlist[,1])

# Step 1: Get the data
data1<-getSymbols(stocklist[1],auto.assign = FALSE)

# Step 2: Create your indicator
data1$RSI14 <- RSI(Cl(data1),n=14)

# Step 3: Construct your trading rule
sig <-Lag(ifelse(data1$RSI14 < 30, 1, ifelse(data1$RSI14 > 70, -1,0)))

# Step 4: The trading rules/equity curve
ret <- ROC(Cl(data1))*sig
#ret <- ret['2009-06-02/2010-09-07']
eq <- exp(cumsum(ret))
plot(eq)


# Step 5: Evaluate strategy performance
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)
