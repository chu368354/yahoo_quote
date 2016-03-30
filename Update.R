library(quantmod)

lookback = 60
startDate<-Sys.Date() - lookback

#create export path
inputpath<-"D:/R/stock/Auto_Stock/csv/"
theFiles<-list.files(path=inputpath,pattern=".csv")

data = read.csv(paste(inputpath,theFiles[1],sep=""))
data = xts(data[,c("open","high","low","close","volume","adj.")],
           order.by = as.Date(data[,"Date"],format="%Y-%m-%d"))
lastHistoricalDate = index(data[nrow(data),])


#sort data
# raw2<-raw1[order(raw1$Date),]

for (i in theFiles){
  data = read.csv(paste(inputpath,i,sep=""))
  data = xts(data[,c("open","high","low","close","volume","adj.")],
             order.by = as.Date(data[,"Date"],format="%Y-%m-%d"))
  lastHistoricalDate = index(data[nrow(data),])
  
  recent = getSymbols(Symbols = substr(i,1,nchar(i)-4), 
                      src = "yahoo", 
                      from = statDate, 
                      auto.assign = FALSE)
  colnames(recent1) = c("open","high","low","close","volume","adj.")
  recent2<-data.frame(recent1,Date=index(raw1))
  pos = match(as.Date(lastHistoricalDate,format="%Y-%m-%d"),as.Date(recent2$Date))
  
  if (!is.na(pos)){ 
    if (pos == nrow(recent))
      print("File already up-to-date")
    
    if (pos < nrow(recent)){
      dt = NULL
      dt = rbind(data,recent[(pos+1):nrow(recent),])
      write.zoo(dt,paste(thePath,i,sep=""),sep=",",row.names=FALSE) 
    }
  }
  
  if (is.na(pos))
    print("Error: dates do not match")
}
