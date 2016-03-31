#Auto download list of historical data from quantmod
setwd("C:/R/yahoo_quote/")

#Clean up all file from folder
file.remove(file.path("C:/R/yahoo_quote/csv/", list.files("C:/R/yahoo_quote/csv/"))) 

startDate<-("2016-01-01")

#create export path
outputpath<-"C:/R/yahoo_quote/csv/"

library(sqldf)
library(xlsx)

#package for RSI
library(TTR)

#package for MACD
library(quantmod)

# create list of all .csv files in folder
rawlist<-read.csv("C:/R/yahoo_quote/list.csv",header=TRUE,sep=",")
stocklist<-as.vector(rawlist[,1])

#Export historical data, with error handle message
for (i in stocklist){
  tryCatch({
    print(i)
    raw1 = getSymbols(Symbols = i, 
                      src = "yahoo", 
                      from = startDate, 
                      auto.assign = FALSE)
    colnames(raw1) = c("Open","High","Low","Close","Volume","Adj")
    raw1$ADX<-ADX(HLC(raw1))
    raw1$slowD<-stoch(HLC(raw1),nFastK = 9, nFastD = 3, nSlowD = 3, maType = "EMA")
    raw1$J<-3*raw1$fastK-2*raw1$slowD
    #change the time series to data frame
    raw2<-data.frame(raw1,Date=index(raw1))
    #add Symbol column
    raw2$Symbol<-as.character(i)
    write.zoo(raw2,paste(outputpath,i,".csv",sep=""),sep=",",row.names=FALSE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
