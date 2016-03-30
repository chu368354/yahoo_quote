#Auto download list of historical data from quantmod
setwd("D:/R/stock/Auto_Stock")

startDate<-("2014-01-01")

#create export path
outputpath<-"D:/R/stock/Auto_Stock/csv/"

library(sqldf)
library(xlsx)

#package for RSI
library(TTR)

#package for MACD
library(quantmod)

# create list of all .csv files in folder
rawlist<-read.csv("D:/R/stock/Auto_Stock/list.csv",header=TRUE,sep=",")
stocklist<-as.vector(rawlist[,1])

#Export historical data, with error handle message
for (i in stocklist){
  tryCatch({
  print(i)
  raw1 = getSymbols(Symbols = i, 
                   src = "yahoo", 
                   from = startDate, 
                   auto.assign = FALSE)
  colnames(raw1) = c("open","high","low","close","volume","adj.")
  #change the time series to data frame
  raw2<-data.frame(raw1,Date=index(raw1))
  #add Symbol column
  raw2$Symbol<-as.character(i)
  write.zoo(raw2,paste(outputpath,i,".csv",sep=""),sep=",",row.names=FALSE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
