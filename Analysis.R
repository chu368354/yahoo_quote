setwd("D:/R/stock/Auto_Stock")
library(sqldf)
library(xlsx)

#package for RSI
library(TTR)

#package for MACD
library(quantmod)

RSIday<<-14
MACDFast<<-12
MACDSlow<<-26
MACDSig<<-9

#create import path
folder<-"D:/R/stock/Auto_Stock/csv/"
# create list of all .csv files in folder
filelist<-list.files(path=folder,pattern = "*.csv")

#check count of the filelist
length(filelist)

# read in each .csv file in file_list and create a data frame with the data of as the .csv file
for(i in 1:length(filelist)){
  assign(filelist[i],
         read.csv(paste(folder,filelist[i],sep = "")))
}

# read in each .csv file in file_list and rbind them into a data frame called raw 
raw1 <- 
  do.call("rbind", 
          lapply(filelist, 
                 function(x) 
                   read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE)))

#Remove all imported data frame
rm(list=ls(pattern = "csv"))

#checking format of each column
sapply(raw1,class)

#formatting date
raw1$Date<-as.Date(raw1$Date,"%Y-%m-%d")

#checking format of each column
sapply(raw1,class)

#use SQL to sort data
raw2<-sqldf("select * from raw1
            where Volume>0
            order by Symbol, Date")

#RSI
raw2$RSI14<-RSI(raw2$Close,n=RSIday)

#MACD
raw2$MACD<-MACD(raw2$Close,nFast = MACDFast, nSlow = MACDSlow, nSig = MACDSig,maType ="EMA", percent = FALSE)

#EMA9
raw2$MACDFast<-EMA(raw2$Close,n=MACDFast)
raw2$MACDSlow<-EMA(raw2$Close,n=MACDSlow)
raw2$MACDdiff<-raw2$MACDFast-raw2$MACDSlow
raw2$Signal<-EMA(raw2$MACDdiff,n=MACDSig)

#Divergence of MACD
raw2$Divergence<-raw2$MACD-raw2$Signal

#Divergence of KDJ
raw2$KDJDivergence<-abs(abs(raw2$fastK-raw2$slowD)-abs(raw2$J))

#use SQL to sort data
raw3<-sqldf("select * from raw2
            order by Symbol, Date desc")

raw3$MaxDate<-max(raw3$Date,na.rm=TRUE)
drops<-c("Index","MACDFast","MACDSlow","MACDdiff","MaxDate","fastD")

raw4<-raw3[raw3$Date==raw3$MaxDate,!names(raw3) %in% drops]

raw5<-sqldf("select * from raw4
            where Volume>=1000000 and Close>1 and
            RSI14<=30 and 
            Divergence between -0.2 and 0.2 and
            KDJDivergence <=0.2
            order by Divergence,KDJDivergence")

raw5<-raw5[,c("Symbol","Date","Open","High","Low","Close","Volume","Adj","RSI14","MACD","Signal","Divergence","DIp","DIn","DX","ADX","fastK","slowD","J","KDJDivergence")]

library(jsonlite)
outputjson<-toJSON(raw5)
outputjson

write(outputjson,"D:/R/stock/Auto_Stock/output.json")

write.csv(raw5,file = "D:/R/stock/Auto_Stock/output.csv", row.names = FALSE)
