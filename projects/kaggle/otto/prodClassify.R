library(caret)
rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\otto\\")
source("../RUtils.R")

# Load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
head(train)
str(train)

sample_sub <- read.csv("sampleSubmission.csv")
# remove id column so it doesn't get picked up by the random forest classifier
train2 <- train[,-1]

library(randomForest)
# set a unique seed number so you get the same results everytime you run the below model,
# the number does not matter
set.seed(12)
# create a random forest model using the target field as the response and all 93 features as inputs
fit <- randomForest(as.factor(target) ~ ., data=train2, importance=TRUE, ntree=100)

# create a dotchart of variable/feature importance as measured by a Random Forest
varImpPlot(fit)

# use the random forest model to create a prediction
pred <- predict(fit,test,type="prob")
submit <- data.frame(id = test$id, pred)
write.csv(submit, file = "firstsubmit.csv", row.names = FALSE)


train$y <- 0
train[which(train$target=='Class_2'),]$y <- 1
str(train)
train <- subset(train, select=-c(id,target))
train$y <- as.factor(train$y)

#Split train and test dataset
trainIndex <- createDataPartition(train$y,p=0.7,list=F,times=1)

train2 <- train[trainIndex,]
test2 <- train[-trainIndex,]


library(randomForest)
# set a unique seed number so you get the same results everytime you run the below model,
# the number does not matter
set.seed(12)
nTrees=500
# create a random forest model using the target field as the response and all 93 features as inputs
rfFit <- randomForest(y ~ ., data=train2, ntree=100)
pred2 <- predict(rfFit, newdata=test2, ntree=100, type="response")
LogLoss(test2$y, pred2)

library(gbm)
gbmFit <- gbm(y~.,data=train2,n.trees=nTrees,cv.folds=5)
pred2 <- predict(gbmFit, newdata=test2, ntree=nTrees, type="response")
LogLoss(test2$y, pred2)

library(C50)
c50Fit <- C5.0(as.factor(y)~.,data=train2, rules=T, trials=10)
cPred2 <- predict(c50Fit,newdata=test2,type="class")
LogLoss(test2$y, as.numeric(as.character(cPred2)))

impFeatures <- function(rfModel,n) {
  ii <- importance(rfModel)
  return (ii[order(ii,decreasing=T)[1:n],])
}

LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}


