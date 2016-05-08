#Auto download list of historical data from quantmod
setwd("D:/R/stock/Auto_Stock")

#create export path
outputpath<-"D:/R/stock/Auto_Stock/csv/"

#Clean up all file from folder
file.remove(file.path(outputpath, list.files(outputpath))) 

startDate<-("2014-01-01")


library(sqldf)
library(xlsx)

#package for RSI
library(TTR)

#package for MACD
library(quantmod)

# create list of all .csv files in folder
rawlist<-read.csv("D:/R/stock/Auto_Stock/list.csv",header=TRUE,sep=",")
class(rawlist)
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
