###################################################
# Instance based Learning
###################################################
#rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\drivers\\")
library(gbm)
library(randomForest)
library(h2o)
library(stringr)
source("driversFUNs.R")


clearH2o <- function() {
  ls_temp <- h2o.ls(localH2o)
  for (n_ls in 1:nrow(ls_temp)) {
    h2o.rm(localH2o, keys = as.character(ls_temp[n_ls, 1]))
  }
}

drivers = list.files("data")
#Sample 100 drivers to work for testing purpose
#drivers=drivers[1:100]
# randomDrivers = sample(drivers, size = 5)
randomDrivers <- c("1174", "1704" ,"3216", "178" , "2737")
refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
  dirPath = paste0("data/", driver, '/')
  #for(i in sample(1:200,100))
  for (i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(addFeatures(trip), target)
    refData = rbind(refData, features)
  }
}
localH2o = h2o.init()


target = 1
names(target) = "target"
submission = NULL
#imp <- data.frame(col=1:(ncol(refData)-1))
for(driver in drivers)
  
# for(driver in c("1"))
{
  print(driver)
  dirPath = paste0("data/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(addFeatures(trip), target)
    currentData = rbind(currentData, features)
  }
  
  currentData = as.data.frame(currentData)
  rownames(currentData) <- NULL
  
  #filteredData <- currentData[which(currentData$tripTime>quantile(currentData$tripTime,0.25)[[1]]),]
  #filteredData <- currentData[which(currentData$tripTime<quantile(currentData$tripTime,0.75)[[1]]),]
  
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  #train$target = as.factor(train$target)
  rownames(train) <- NULL
  #  train <- binFeatures(train, 1:(ncol(train)-1))
  # colnames(train) <- c(paste0("col_", 1:(ncol(train)-1)), "target")
  
  # Code to identify sureshot current Drivers and 
  #   g = NULL
  #   g = randomForest(target ~ ., 
  #                   data=train, ntree=500)
  #   p =predict(g, currentData, n.trees=500, type="prob")
  #   p = p[,2]
  #   currDriverData <- currentData[order(p)[101:200],]
  #   train = rbind(currDriverData, refData)
  
  
  train$target = as.factor(train$target)
  train.h2o = as.h2o(localH2o,train,key="train.h2o")
  driver.deep.model = h2o.deeplearning(y = "target", x = colnames(train[,1:ncol(train)-1]), 
                                       data = train.h2o, hidden = c(200), variable_importances = TRUE, classification = TRUE,
                                       autoencoder = TRUE, epochs = 5)
  #p1 <-  as.matrix(h2o.predict(driver.deep.model, as.h2o(localH2o,currentData)))
  
  deep.features.train <- h2o.deepfeatures(train.h2o[,1:(ncol(train)-1)], driver.deep.model)
  train.h2o <- cbind(train.h2o,deep.features.train)
  
  #   test.h2o <- as.h2o(localH2o,currentData,key="test.h2o")
  #   deep.features.test <- h2o.deepfeatures(test.h2o[,1:(ncol(test.h2o)-1)], driver.deep.model)
  #   test.h2o <- cbind(test.h2o, deep.features.test)
  
  cols <- colnames(train.h2o)
  #train.h2o$target <- as.factor(train.h2o$target)
  #   model <- h2o.gbm(y="target", x=cols[cols!="target"],
  #                    data=train.h2o, # dataset
  #                    distribution="bernoulli", # see the help for other choices
  #                    n.trees=500, # number of trees)
  #                    shrinkage=0.01, # shrinkage or learning rate,
  #                    # 0.001 to 0.1 usually work
  #                    importance=T,
  #                    interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
  #                    #bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
  #                    n.minobsinnode = 10, # minimum total weight needed in each node
  #   ) # don't print out progressplot 
  #   p <- h2o.predict(model,newdata=test.h2o)
  #   p <- round((as.matrix(p)[,3]), digits=3)
  
  train.new <- data.frame(as.matrix(train.h2o))
  test.new <- train.new[1:200,]
  
  #   write.csv(train.new, paste0(dirPath,"deepTrain.csv"), quote=F, row.names=F)
  clearH2o()
  model <- gbm(target~.,
               data=train.new, # dataset
               distribution="bernoulli", # see the help for other choices
               n.trees=500, # number of trees)
               shrinkage=0.01, # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
               n.minobsinnode = 10, # minimum total weight needed in each node
               verbose=FALSE) # don't print out progressplot 
  p <- predict(model,newdata=test.new, 
               type="response", 
               n.trees=500)
  p <- round(p, digits=3)
  #   #imp <- cbind(imp, data.frame(importance(g)))
  #g = gbm(target ~ ., data=train, distribution = "bernoulli", n.trees=100, verbose = T, bag.fraction = 0.75, cv.folds = 5, interaction.depth = 5)
  #   currentData = as.data.frame(currentData)
  #   rownames(currentData) <- NULL
  #   currentData$target = as.factor(currentData$target)
  #   colnames(currentData) <- c(paste0("col_", 1:(ncol(currentData)-1)), "target")
  #currentData <- train[1:nrow(currentData),]
  #p =predict(g, currentData, n.trees=500, type="prob")
  #p = p[,2]
  #   pq = quantile(p,c(0.05,1))
  #   p[p<pq[[1]]] <- 0
  #   p[p>=pq[[1]]] <- 1
  
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "results.csv", row.names=F, quote=F)
# 
#extract driverId column
#results$driverId <- apply(results, 1, function(x) return(strsplit(as.character(x[1]), "_")[[1]][1]))

#Scale the probs - did not help improve scores at all
# groupAgg <- sqldf("select driverId, min(prob) as lo, max(prob) as hi from results group by driverId")
# newResults <- sqldf("select res.driver_trip, (res.prob-agg.lo)/(agg.hi-agg.lo) as prob from results res, groupAgg agg where agg.driverId = res.driverId")
# write.csv(newResults, "newresults.csv", row.names=F, quote=F)