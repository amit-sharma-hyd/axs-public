###################################################
# Instance based Learning
###################################################
#rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\drivers\\")
library(gbm)
library(randomForest)
source("driversFUNs.R")

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

target = 1
names(target) = "target"
submission = NULL
#imp <- data.frame(col=1:(ncol(refData)-1))
for(driver in drivers[1501:2736])
# for(driver in c("16"))
#driver <- "2402"
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
  
  train <- train[!duplicated(train[,1:(ncol(train)-1)]),]
  
  #train$target = as.factor(train$target)
  rownames(train) <- NULL
  #  train <- binFeatures(train, 1:(ncol(train)-1))
  # colnames(train) <- c(paste0("col_", 1:(ncol(train)-1)), "target")
  #g = NULL
  #g = randomForest(target ~ ., 
  #                 data=train, ntree=500)
  nTrees = 500
  model <- gbm(target~.,
               data=train, # dataset
               distribution="bernoulli", # see the help for other choices
               n.trees=nTrees, # number of trees)
               shrinkage=0.005, # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=9, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
               n.minobsinnode = 10, # minimum total weight needed in each node
               cv.folds=5,
               verbose=F) # don't print out progressplot 
  p <- predict(model,newdata=currentData, 
               type="response", 
               n.trees=nTrees)
  p <- round(p, digits=3)
  #imp <- cbind(imp, data.frame(importance(g)))
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
write.csv(submission, "results_1501_2736_3_14.csv", row.names=F, quote=F)

#extract driverId column
#results$driverId <- apply(results, 1, function(x) return(strsplit(as.character(x[1]), "_")[[1]][1]))

#Scale the probs - did not help improve scores at all
# groupAgg <- sqldf("select driverId, min(prob) as lo, max(prob) as hi from results group by driverId")
# newResults <- sqldf("select res.driver_trip, (res.prob-agg.lo)/(agg.hi-agg.lo) as prob from results res, groupAgg agg where agg.driverId = res.driverId")
# write.csv(newResults, "newresults.csv", row.names=F, quote=F)