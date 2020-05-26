library(gbm)
library(VIM)
library(vegan)
# library(sampling)
#library(neuralnet)

rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\higgs")

source("./higgsFUNs.R")
#Treat -999 as NA
train <- read.csv("training.csv")

#Mark values=-999 as NA
train[train==-999] <- NA

#Add a colunm for logit classification
train$Signal <- 0
train$Signal[which(train$Label=="s")] <- 1
#train$Signal <- as.factor(train$Signal)
#train$Label <- NULL

#trainCC <- train[complete.cases(train),]

#Divide the data into training and test
# sampleSize <- floor(0.7*c(nrow(train[train$Signal==0,]), nrow(train[train$Signal==1,])))
# samples <- strata(train, 
#                   stratanames=c("Signal"), 
#                   size=c(22325, 25354),
#                   method="srswor")

# trgData <- getdata(train, samples)
# trgData <- subset(trgData, select=-c(ID_unit,Prob,Stratum))
# testData <- train[-(as.numeric(rownames(trgData))),]


trgData <- stratified(df=train, id=1, group=ncol(train), size=.7, seed=123)
testData <- train[-(as.numeric(rownames(trgData))),]

table(trgData$Signal)
table(testData$Signal)

#Standardize the data
trgDataTemp <- decostand(subset(trgData, select=(-c(EventId, Signal, Label))), method="standardize" )
trgDataTemp <- imputeUsingLinReg(trgDataTemp)
trgData <- trgDataTemp
# trgData <- cbind(trgDataTemp, trgData$Signal)
# names(trgData)[[ncol(trgData)]] <- "Signal"

#Extract result set data
resData <- subset(testData, select=(c(EventId, Label, Weight)))
colnames(resData) <- c("EventId", "Label", "Weight")
write.csv(resData, "resFile.csv", row.names=FALSE, quote=FALSE)

#Standardize test data
testDataTemp <- decostand(subset(testData, select=(-c(EventId, Label))), method="standardize" )
testDataTemp <- imputeUsingLinReg(testDataTemp)
testData <- testDataTemp
# testData <- cbind(testDataTemp, testData$Signal)
# names(testData)[[ncol(testData)]] <- "Signal"


#rm(train, samples, trgDataTemp)

#Impute missing data
#aggr(trgData, plot=FALSE)
#aggr(testData, plot=FALSE)
#trgData <- imputeMedian(trgData)

# Start GLM - Logistic
#model <- glm(Signal~.,data=trgData, family=binomial(logit))
#summary(model)


# Begin - LM
model <- lm(Weight~.,data=trgData)
summary(model)
prob<-predict(model,newdata=testData) 

summary(prob)


getThresholdWeight(-10,200,prob,resData)

ot <- 0
resData$Prob <- prob
resData$Class <- 'b'
resData$Class[resData$Prob < 0] <- 's'

table(resData$Class)

hist(resData$Prob)

outData <- resData
outData <- outData[order(outData$Prob, decreasing=T),]
head(outData)
outData$RankOrder <- 1:nrow(outData)
outData <- subset(outData, select=c(EventId, RankOrder,Class))
head(outData)
write.csv(outData, "outFile.csv",row.names=FALSE, quote=FALSE)



  nrow(resData)
nrow(outData)

# plotAcc(-20.1,200,prob,testData$Signal)
# 
# plotAMS(-10,2,prob,resData)
resData$Prob <- prob
resData$Class[which(resData$Prob<17.2)] <- 's'
resData$Class[which(resData$Prob>=17.2)] <- 'b'


pred_class <- data.frame(rep(0, length(prob)))
pred_class[which(prob < -8.67)] <- 1
colnames(pred_class) <- "Pred"
pred_class <- cbind(pred_class,testData$Signal)
table(pred_class)
#head(pred_class,30)

#trainProb <- predict(model,trainStd,type="response")
#trainOut <- data.frame(trainProb)
#trainOut <- cbind(trainOut, trainStd$Signal)
#str(trainOut)
#boxplot(trainProb~trainStd$Signal, data=trainOut)


#Standardize the data
testDataTemp <- decostand(subset(testData, select=(-c(EventId, Signal, Weight))), method="standardize" )
testDataTemp <- imputeUsingLinReg(testDataTemp)
testDataTemp <- cbind(testData$EventId, testDataTemp)
names(testDataTemp)[1] <- "EventId"
testDataTemp <- cbind(testDataTemp, testData$Signal)
names(testDataTemp)[ncol(testDataTemp)] <- "Signal"
testData <- testDataTemp

head(testData)



#Impute missing values
#testData <- imputeMedian(testData)

predProb <- predict(model,testData,type="response")

predClass <- rep(0,length(predProb))
predClass[which(predProb>0.5)] <- 1

#Test the training algo
table(testData$Signal==predClass)[[2]]*100/length(predClass)



#File generation logic
test <- read.csv("test.csv")
#Mark values=-999 as NA
test[test==-999] <- NA

testData <- test
testDataTemp <- decostand(subset(testData, select=(-c(EventId))), method="standardize" )
testDataTemp <- imputeUsingLinReg(testDataTemp)
testDataTemp <- cbind(testData$EventId, testDataTemp)
names(testDataTemp)[1] <- "EventId"
testData <- testDataTemp

head(testData)


predProb <- predict(GBM,testData,type="response",n.trees=numTrees)

predClass <- rep(0,length(predProb))
predClass[which(predProb>0.39)] <- 1
out <- subset(testData,select=c(EventId))
out <- cbind(out, predProb)
out <- cbind(out, predClass)


out <- out[order(predProb),]
out$RankOrder <- c(1:nrow(out))
out$Class <- rep("b", nrow(out))
out$Class[which(out$predClass==1)] <- "s"
out <- subset(out,select=c(EventId, RankOrder, Class))
head(out)
write.csv(out, "out.csv",row.names=FALSE, quote=FALSE)


