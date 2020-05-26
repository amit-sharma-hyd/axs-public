library(sqldf)
library(gbm)
library(C50)

#Factorize columns
factorCols <- c("customer_ID", "record_type", "day", "costDir", "costDir1",
                "state", "location", "group_size", "homeowner", "risk_factor",
                "married_couple", "costBin", "carAgeBin", "ageOldBin", "ageYngBin",
                "C_previous",LETTERS[1:7],paste0(LETTERS[1:7],1),
                paste0(LETTERS[1:7],2), paste0(LETTERS[1:7],9))

getLastQuoted <- function(trainData) {
  trainTrain <- trainData[which(trainData$record_type==0),]
  
  trainTrainMaxSP <- sqldf("SELECT customer_ID,A `A1`,B `B1`,C `C1`,D `D1`,E `E1`, F `F1`,G `G1`
                           FROM trainTrain GROUP BY customer_ID HAVING shopping_pt = max(shopping_pt)")
  return(trainTrainMaxSP)
}

appendLast2Quotes <- function(trainData) {
  trainTrain <- trainData[which(trainData$record_type==0),]
  
  trainTrainMaxSP <- sqldf("SELECT * FROM trainData GROUP BY customer_ID HAVING shopping_pt = max(shopping_pt)")
  
  #Take the last shopping point before purchase
  trainTrainMaxSP_1 <- sqldf("SELECT customer_ID,A `A1`,B `B1`,C `C1`,D `D1`,E `E1`, F `F1`,G `G1`, cost `cost1` 
                             FROM trainTrain GROUP BY customer_ID HAVING shopping_pt = max(shopping_pt)")
  
  #Take the last but one shopping point before purchase
  trainTrainMaxSP_2 <- sqldf("SELECT customer_ID,A `A2`,B `B2`,C `C2`,D `D2`,E `E2`, F `F2`,G `G2`, cost `cost2`
                             FROM trainTrain t1 WHERE shopping_pt = 
                             (select max(shopping_pt)-1 from trainTrain t2 where t2.customer_ID=t1.customer_ID)
                             GROUP BY customer_ID")
  
  cost3 <- sqldf("SELECT customer_ID, cost `cost3`
                 FROM trainTrain t1 WHERE shopping_pt = 
                 (select max(shopping_pt)-2 from trainTrain t2 where t2.customer_ID=t1.customer_ID)
                 GROUP BY customer_ID")
  
  ret <- merge(trainTrainMaxSP_1, trainTrainMaxSP_2, by.x="customer_ID", by.y="customer_ID")
  ret <- merge(trainTrainMaxSP,ret,by.x="customer_ID", by.y="customer_ID")
  ret <- merge(cost3,ret,by.x="customer_ID", by.y="customer_ID",all.y=TRUE)
  
  return(ret)
}


getMaxFreqQuotes <- function(train) {
  trainTrain <- train[which(train$record_type==0),]
  
  trainTrainOut <- subset(trainTrain,select=c(customer_ID,A,B,C,D,E,F,G))
  
  predCols <- c("A","B","C","D","E","F","G")
  ret <- sqldf("select DISTINCT customer_ID from trainTrainOut")
  for (predCol in predCols) {
    
    Counts <- sqldf(paste0("select customer_ID, ", predCol, 
                           ", count(*) `Acount` FROM trainTrainOut group by customer_ID,", predCol))
    MaxCounts <- sqldf("select customer_ID, max(Acount) `max_Acount` from Counts group by customer_ID")
    
    max <- sqldf(paste0("select max(t1.", predCol, ") `", predCol, "9` from Counts t1, MaxCounts t2 
                        where t1.customer_ID=t2.customer_ID
                        and t1.Acount = t2.max_Acount
                        group by t1.customer_ID")) 
    
    ret <- cbind(ret, max)
  }
  return(ret)
}

augmentData <- function(baseData) {
  
  dataAug <- appendLast2Quotes(baseData)
  
  dataAug$costDir <- sign(dataAug$cost1-dataAug$cost2)
  dataAug$costDir1 <- sign(dataAug$cost2-dataAug$cost3)
  
  dataAug <- merge(dataAug, getMaxFreqQuotes(baseData), by.x="customer_ID", by.y="customer_ID")
  
  library(infotheo)
  carAgeBin <- discretize(dataAug$car_age,disc="equalwidth",nbins=20)
  dataAug$carAgeBin <- apply(carAgeBin,1,function(x) if (x>5) 1 else (x))
  
  costBin <- discretize(dataAug$cost,disc="equalwidth",nbins=6)
  colnames(costBin) <- c("costBin")
  dataAug <- cbind(dataAug,costBin)
  
  dataAug$ageOldBin <- as.factor(discretize(dataAug$age_oldest,disc="equalwidth", nbins="10")$X)
  dataAug$ageYngBin <- as.factor(discretize(dataAug$age_youngest,disc="equalwidth", nbins="10")$X)
  
  dataAug[,factorCols] <- lapply(dataAug[,factorCols],factor)
  
  return(dataAug)
  
}

augmentTrainTestData <- function() {
  train <- read.csv("train.csv")
  write.csv(augmentData(train), "trainAug.csv")
  rm(train)
  test <- read.csv("test_v2.csv")
  write.csv(augmentData(test), "testAug.csv")
}

loadAugTrainData <- function() {
  trainAug <- read.csv("trainAug.csv")
  trainAug[,factorCols] <- lapply(trainAug[,factorCols],factor)
  return(trainAug)
}

loadAugTestData <- function() {
  testAug <- read.csv("testAug.csv")
  testAug[,factorCols] <- lapply(testAug[,factorCols],factor)
  return(testAug)
}

#########################################################################
#########                   BEGIN : GBM                     #############
#########################################################################

getGBMModel <- function(train) {
  library(gbm)
  set.seed(1234)
  model <- gbm(G ~ state+homeowner+car_value+risk_factor+married_couple+carAgeBin
               +C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2
               +A9+B9+C9+D9+E9+F9+G9, # formula
               data= train, # dataset
               distribution="multinomial", # see the help for other choices
               n.trees=1000, # number of trees)
               shrinkage=0.01, # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
               n.minobsinnode = 10, # minimum total weight needed in each node
               verbose=FALSE) # don't print out progressplot
  return(model)
}

getGBMPred <- function(model, test) {
  prob<-as.data.frame(predict(model,newdata=test, 
                              type="response", 
                              n.trees=1000)) 
  colnames(prob) <- 1:4
  pred_class <- apply(prob,1,function(x) return(names(which.max(x))[1]))
  
  return(pred_class)
}

#########################################################################
#########                    END  : GBM                     #############
#########################################################################


#########################################################################
#########                    BEGIN: C50                     #############
#########################################################################

getC50Model <- function(train) {
  dtC50= C5.0(G ~ state+homeowner+car_value+risk_factor+married_couple+carAgeBin
              +C_previous+shopping_pt+costBin
              +A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+A9+B9+C9+D9+E9+F9+G9, 
              data=train, rules=TRUE, na.action="na.pass",trials=10)
  return (dtC50)
}

getC50Pred <- function(model, test) {
  dtC50PredG <- predict(model, newdata=test,type="class")
  return (dtC50PredG)
}


#########################################################################
#########                    END  : C50                     #############
#########################################################################

#########################################################################
#########                   BEGIN : RF                      #############
#########################################################################

getRFModelForG <- function(train) {
  
  print("Training model...")
  modelRF <- randomForest(G~state+homeowner+car_value+risk_factor+married_couple+carAgeBin
                          +C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2
                          +A9+B9+C9+D9+E9+F9+G9, data=train, ntree=300, na.action=na.omit)
  print("Training model...Completed!")
  
  return (modelRF)
  
}

evalRFModel <- function(model, test) {
  pred <- data.frame(predict(modelRF, subset(rfTrainData, select=-c(G))))
  colnames(pred) <- c("pred")
  pred$pred[which(is.na(pred$pred))] <- rfTrainData$G1[which(is.na(pred$pred))]
  return(pred)
}

#########################################################################
#########                   END : RF                        #############
#########################################################################




loadTestData <- function() {
  test <- read.csv("test_v2.csv")
  
  testAug <- appendLast2Quotes(test)
  testAug <- merge(testAug, getMaxFreqQuotes(test), by.x="customer_ID", by.y="customer_ID")
  
  testAug$costDir <- sign(testAug$cost1-testAug$cost2)
  testAug$costDir1 <- sign(testAug$cost2-testAug$cost3)
  
  
  library(infotheo)
  carAgeBin <- discretize(testAug$car_age,disc="equalwidth",nbins=20)
  testAug$carAgeBin <- apply(carAgeBin,1,function(x) if (x>5) 1 else (x))
  
  costBin <- discretize(testAug$cost,disc="equalwidth",nbins=6)
  colnames(costBin) <- c("costBin")
  testAug <- cbind(testAug,costBin)
  

  testAug[,factorCols] <- lapply(testAug[,factorCols],factor)
  
  return(testAug)
}


generateGBMFullResults <- function(train, test, outVars, numTrees) {
  model <- c()
  pred <- test$customer_ID
  for (outVar in outVars) {
    print(paste("Predicting:", outVar))
    rm(model)
    model <- gbm(as.formula(paste0(outVar,"~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+costDir+costDir1")),
                 data= train, # dataset
                 distribution="multinomial", # see the help for other choices
                 n.trees=numTrees, # number of trees)
                 shrinkage=0.01, # shrinkage or learning rate,
                 # 0.001 to 0.1 usually work
                 interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
                 bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
                 n.minobsinnode = 10, # minimum total weight needed in each node
                 verbose=FALSE) # don't print out progressplot               
    prob<-as.data.frame(predict(model,newdata=test, 
                                type="response", 
                                n.trees=numTrees)) 
    colnames(prob) <- levels(test[,outVar])
    pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
    colnames(pred_class) <- outVar
    write.csv(pred_class,paste0("GBM_",outVar,".csv"),row.names=FALSE, quote=FALSE)
  }
  
  #Read from files and aggregate the data
  pred <- data.frame(test$customer_ID)
  for (outVar in outVars) {
    varPred <- read.csv(paste0("GBM_",outVar,".csv"))
    pred <- cbind(pred,varPred)
  }
  colnames(pred) <- c("customer_ID",LETTERS[1:7])
  return(pred)
}

collapsePreds <- function(pred) {
  plans <- subset(pred,select=c(2:8))
  predOut <- cbind(as.character(pred$customer_ID), apply(plans,1,paste0,collapse=""))
  predOut <- data.frame(predOut)
  colnames(predOut) <- c("customer_ID", "plan")
  predOut$plan <- as.character(predOut$plan)
  return(predOut)
}

evalTrainTestResults <- function(preds,testRecs) {
  for (predVar in LETTERS[1:7]) {
    predCorrect <- length(which(preds[,predVar] == testRecs[,predVar]))
    lastQuotCorrect <- length(which(testRecs[,paste0(predVar,1)] == testRecs[,predVar]))
    print(paste(predVar,":",predCorrect,lastQuotCorrect))
  }
  
  print("Collapsed Results Comparision")
  
  predOut <- collapsePreds(preds)
  lastQuot <- collapsePreds(subset(testRecs, select=c("customer_ID", paste0(LETTERS[1:7],1))))
  actual <- collapsePreds(subset(testRecs, select=c("customer_ID", LETTERS[1:7])))
  print(paste("All:", length(which(predOut$plan == actual$plan)), length(which(lastQuot$plan == actual$plan)))) 
}
