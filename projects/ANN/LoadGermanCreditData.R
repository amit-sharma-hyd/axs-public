rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/20140208-ANN/rcode_Neural-Networks")

library(gdata)
gc1<-read.xls('./German-Credit2.xls', sheet="Part1")
gc2<-read.xls('./German-Credit2.xls', sheet="Part2")

gc <- merge(gc1, gc2, by="OBS")

library(dummies)

mstatus <- read.table('./mstatus.csv', 
                      header=T,sep=',', dec=".", 
                      na.strings="NA")

# str(mstatus)
# summary(mstatus)
# dummy.status <- dummy(mstatus$mstatus)
# str(dummy.status)
# colnames(dummy.status) <- c("MALE_DIV", "MALE_MAR_or_WID", "MALE_SINGLE")

library(reshape)
casted <- cast(mstatus,OBS~mstatus, val="value")
gc <- merge(gc, casted, by="OBS")

str(gc)

gc <- merge(gc, mstatus, by="OBS")

gc <- gc[!(colnames(gc) %in% c("mstatus", "value"))]

#gc <- data.frame(lapply(gc, as.factor))

#Remove NAs!!!
gc <- gc[!is.na(gc$RESPONSE),]

#Create training and testing samples
library(sampling)
set.seed(24)
#Check no of RESPONSE class variables and their values
table(gc$RESPONSE)


gc <- gc[complete.cases(gc),]
gc <- decostand(gc,na.rm=T, "range") # to standardize the data using 'Range' method

samples <- strata(gc, stratanames=c("RESPONSE"), size=c(0.6*2100, 0.6*900), method="srswor")
gc.train <- getdata(gc,samples)
#Get rid of last 3 cols introduced by sampling
gc.train <- gc.train[,2:(ncol(gc.train)-3)]
gc.test <- getdata(gc, -samples$ID_unit)

#Some cleanup
rm(samples, casted, mstatus, gc1, gc2)

str(gc.train)

