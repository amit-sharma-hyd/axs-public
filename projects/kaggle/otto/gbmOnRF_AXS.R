library(caret)
rm(list=ls())
source("../RUtils.R")
setwd("C:\\axs\\work\\kaggle\\otto\\")
#Splits the training data into 2 sets,
#retaining the class distribution. Builds a
#randomForest on the first set, and a gbm
#on the randomForest predictions for the 
#second set. Play with below parameters to
#improve performace - my best LB score using
#this approach is 0.49635

#The evaluation metric function used by caret to score
#a fold. Not optimal - needs vectorization
MCLogLoss <- function(data, lev = NULL, model = NULL)  {
  
  obs <- model.matrix(~data$obs - 1)
  preds <- data[, 3:ncol(data)]
  
  err = 0
  for(ob in 1:nrow(obs))
  {
    for(c in 1:ncol(preds))
    {
      p <- preds[ob, c]
      p <- min(p, 1 - 10e-15)
      p <- max(p, 10e-15)
      err = err + obs[ob, c] * log(p)
    }
  }
  
  out <- err / nrow(obs) * -1
  names(out) <- c("MCLogLoss")
  out
}

#TUNE HERE
#How much of the data to use to build the randomForest
TRAIN_SPLIT = 0.7
RF_MTRY = 9
RF_TREES = 125
GBM_IDEPTH = 4
GBM_SHRINKAGE  = 0.1
GBM_TREES = 50
#TUNE HERE

library(caret)
#Prepare training\testing data, extract
#target and save test id's
trainAll <- read.csv("train.csv") 

trainAll[,2:94] <- sqrt(trainAll[,2:94]) + 3/8

#Create train and test splits
trainIndex <- createDataPartition(trainAll$target, p = TRAIN_SPLIT, list = F, times = 1) 
train <- trainAll[trainIndex,]
test <- trainAll[-trainIndex,]

train <- train[, -which(names(train)=="id")] 
target <- train$target 
train <- train[, -which(names(train)=="target")] 
#test <- read.csv("test.csv") 
id <- test$id 
test <- test[, -which(names(test)=="id")] 

#Split training data into two sets(keep class distribution)
set.seed(20739) 
trainIndex <- createDataPartition(target, p = TRAIN_SPLIT, list = TRUE, times = 1) 
allTrain <- train 
allTarget <- target 
train <- allTrain[trainIndex$Resample1, ] 
train2 <- allTrain[-trainIndex$Resample1, ] 
target <- allTarget[trainIndex$Resample1] 
target2 <- allTarget[-trainIndex$Resample1] 

#Build a randomForest using first training set
fc <- trainControl(method = "repeatedCV", 
                   number = 2, 
                   repeats = 1, 
                   verboseIter=FALSE, 
                   returnResamp="all", 
                   classProbs=TRUE) 
tGrid <- expand.grid(mtry = RF_MTRY) 
model <- train(x = train, y = target, method = "rf", 
               trControl = fc, tuneGrid = tGrid, metric = "Accuracy", ntree = RF_TREES) 
#Predict second training set, and test set using the randomForest
train2Preds <- predict(model, train2, type="prob") 
testPreds <- predict(model, test, type="prob")
model$finalModel

#Build a gbm using only the predictions of the
#randomForest on second training set
fc <- trainControl(method = "repeatedCV", 
                   number = 10, 
                   repeats = 1, 
                   verboseIter=FALSE, 
                   returnResamp="all", 
                   classProbs=TRUE, 
                   summaryFunction=MCLogLoss,) 
tGrid <- expand.grid(interaction.depth = GBM_IDEPTH, shrinkage = GBM_SHRINKAGE, n.trees = GBM_TREES) 
model2 <- train(x = train2Preds, y = target2, method = "gbm", 
                trControl = fc, tuneGrid = tGrid, metric = "MCLogLoss", verbose = FALSE)
model2
hist(model2$resample$MCLogLoss)

#Build submission
submit <- predict(model2, testPreds, type="prob") 
# shrink the size of submission
submit <- format(submit, digits=2, scientific = FALSE)
#submit <- cbind(id=1:nrow(testPreds), submit) 
#write.csv(submit, "submit.csv", row.names=FALSE, quote=F)