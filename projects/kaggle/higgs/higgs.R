library(gbm)
library(VIM)
library(infotheo)
library(vegan)
library(randomForest)
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


trgData <- stratified(df=train, id=1, group=ncol(train), size=0.7, seed=123)
testData <- train[-(as.numeric(rownames(trgData))),]

table(trgData$Signal)
table(testData$Signal)

#Standardize the data
trgDataTemp <- decostand(subset(trgData, select=(-c(EventId, Weight, Signal, Label))), method="standardize" )
trgDataTemp <- imputeUsingLinReg(trgDataTemp)
trgData <- cbind(trgDataTemp, trgData$Signal)
names(trgData)[[ncol(trgData)]] <- "Signal"

#Extract result set data
resData <- subset(testData, select=(c(EventId, Weight, Label)))
colnames(resData) <- c("EventId", "Weight", "Label")


#Standardize test data
testDataTemp <- decostand(subset(testData, select=(-c(EventId, Weight, Signal, Label))), method="standardize" )
testDataTemp <- imputeUsingLinReg(testDataTemp)
testData <- cbind(testDataTemp, testData$Signal)
names(testData)[[ncol(testData)]] <- "Signal"


trgData <- cutVars(trgData)
testData <- cutVars(testData)
#rm(train, samples, trgDataTemp)

#Impute missing data
#aggr(trgData, plot=FALSE)
#aggr(testData, plot=FALSE)
#trgData <- imputeMedian(trgData)

# Start GLM - Logistic
#model <- glm(Signal~.,data=trgData, family=binomial(logit))
#summary(model)

# Begin - NN
#paste0(colnames(trgData), collapse='+')
#[1] "DER_mass_MMC+DER_mass_transverse_met_lep+DER_mass_vis+DER_pt_h+DER_deltaeta_jet_jet+DER_mass_jet_jet+DER_prodeta_jet_jet+DER_deltar_tau_lep+DER_pt_tot+DER_sum_pt+DER_pt_ratio_lep_tau+DER_met_phi_centrality+DER_lep_eta_centrality+PRI_tau_pt+PRI_tau_eta+PRI_tau_phi+PRI_lep_pt+PRI_lep_eta+PRI_lep_phi+PRI_met+PRI_met_phi+PRI_met_sumet+PRI_jet_num+PRI_jet_leading_pt+PRI_jet_leading_eta+PRI_jet_leading_phi+PRI_jet_subleading_pt+PRI_jet_subleading_eta+PRI_jet_subleading_phi+PRI_jet_all_pt+Signal"
# net.sqrt <- neuralnet(Signal~PRI_tau_eta+PRI_tau_phi+PRI_lep_pt+PRI_lep_eta+PRI_lep_phi+
#                         PRI_met+PRI_met_phi+PRI_met_sumet+PRI_jet_num+PRI_jet_leading_pt+
#                         PRI_jet_leading_eta+PRI_jet_leading_phi+PRI_jet_subleading_pt+
#                         PRI_jet_subleading_eta+PRI_jet_subleading_phi+PRI_jet_all_pt,
#                       data=trgData, hidden=10, threshold=0.01)
# print(net.sqrt)

set.seed("1234")
numTrees <- 300

# modelRF <- randomForest(Signal~., data=trgData, 
#                         ntree=numTrees)

GBM <- gbm(Signal~.,
            data= trgData, # dataset
            distribution="bernoulli", # see the help for other choices
            n.trees=numTrees, # number of trees)
            shrinkage=0.01, # shrinkage or learning rate,
           cv.folds=5,
            # 0.001 to 0.1 usually work
            interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
            bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
            n.minobsinnode = 10, # minimum total weight needed in each node
            verbose=TRUE) # don't print out progressplot 

# BEGIN - Try CART
# library(C50)
# 
# trgData$Signal <- as.factor(trgData$Signal)
# dtC50= C5.0(Signal ~ ., 
#             data = trgData, 
#             rules=TRUE)
# summary(dtC50)
# C5imp(dtC50, pct=TRUE)
# 
# prob<-predict(dtC50,newdata=testData, 
#               type="class") 
# 
# plotAMS(-5,200,prob,resData)

# END - Try CART




  #gbm.perf(model)
prob<-predict(GBM,newdata=testData, 
                           type="response", 
                           n.trees=numTrees) 

plotAMS(-5,200,prob,resData)


vpred_class <- data.frame(prob>0.7)
pred_class[pred_class==TRUE] <- 1
pred_class[pred_class==FALSE] <- 0
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
predClass[which(predProb>0.4)] <- 1
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
nrow(out)

