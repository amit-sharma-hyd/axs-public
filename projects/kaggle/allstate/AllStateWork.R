rm(list=ls(all=TRUE))
setwd("C:/axs/work/kaggle/allstate/")
source("./AllStateFUNs.R")

#  augmentTrainTestData()
trainAug <- loadAugTrainData()
testAug <- loadAugTestData()

library(infotheo)
dp <- discretize(trainAug$duration_previous, disc="equalwidth", nbins=15)
trainAug$duration_prev_disc <- dp$X

dp <- discretize(testAug$duration_previous, disc="equalwidth", nbins=15)
testAug$duration_prev_disc <- dp$X

head(dp)

trainAug$CG <- paste0(trainAug$C, trainAug$G)
trainAug$CG <- as.factor(trainAug$CG)

trainAug$CG1 <- paste0(trainAug$C1, trainAug$G1)
trainAug$CG1 <- as.factor(trainAug$CG1)

trainAug$CG2 <- paste0(trainAug$C2, trainAug$G2)
trainAug$CG2 <- as.factor(trainAug$CG2)

#Testing the model using training data (70/30)
totalRows <- nrow(trainAug)
trainCustIds <- sample(unique(trainAug$customer_ID),size=0.7*totalRows,replace=F)
trainRecs <- trainAug[which(trainAug$customer_ID %in% trainCustIds),]
testRecs <- trainAug[which(!(trainAug$customer_ID %in% trainCustIds)),]
rm(trainCustIds, totalRows)

numTrees <- 500

# preds <- generateGBMFullResults(trainRecs,testRecs,300)
outVar <- "G"

set.seed("1234")

#model <- gbm(as.formula(paste0(outVar,"~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+costDir+costDir1")),
 model <- gbm(as.formula(paste0(outVar,"~", paste0(c("state+homeowner+car_value+risk_factor+duration_prev_disc",
                                                   "married_couple+carAgeBin+C_previous+shopping_pt",
                                                   "costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2",
                                                   "costDir+costDir1"),collapse="+"))),
#                "costDir+costDir1"),collapse="+"))),
#            cv.folds = 3,

model <- gbm(as.formula(paste0(outVar,"~", paste0(c("state+risk_factor",
#                                                     "married_couple+carAgeBin+C_previous+shopping_pt",
                                                    "C1+C2"),collapse="+"))),
             data= trainRecs, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=numTrees, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot 

prob<-as.data.frame(predict(model,newdata=testAug, 
                            type="response", 
                            n.trees=numTrees)) 
colnames(prob) <- levels(testAug[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar


prob<-as.data.frame(predict(model,newdata=testRecs, 
                            type="response", 
                            n.trees=numTrees)) 
colnames(prob) <- levels(testRecs[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar

gBoost <- subset(testAug, select=c("customer_ID", paste0(LETTERS[1:7],1)))
gBoost$G1 <- pred_class$G
gBoostOut <- collapsePreds(gBoost)


predComb <- subset(testRecs, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predComb$C1 <- as.factor(substr(pred_class$CG,1,1))
levels(predComb$A1) <- levels(testRecs$A1)
predComb$G1 <- as.factor(substr(pred_class$AG,2,2))
levels(predComb$G1) <- levels(testRecs$G1)

predCombOut <- collapsePreds(predComb)

lastQuot <- collapsePreds(subset(testRecs, select=c("customer_ID", paste0(LETTERS[1:7],1))))

actual <- subset(testRecs, select=c("customer_ID", LETTERS[1:7]))
actualOut <- collapsePreds(subset(testRecs, select=c("customer_ID", LETTERS[1:7])))

print(paste("CG:", length(which(predCombOut$plan == actualOut$plan)), length(which(lastQuot$plan == actualOut$plan)))) 
print(paste("G:", length(which(gBoostOut$plan == actualOut$plan)), length(which(lastQuot$plan == actualOut$plan)))) 


##################################################################################################
#                          BEGIN : Collab Boosted for submission                                 #
##################################################################################################


rm(list=ls(all=TRUE))
setwd("~/work/kaggle/allstate/")
source("./AllStateFUNs.R")

#augmentTrainingData()
trainAug <- loadAugTrainingData()
testAug <- loadTestData()


# preds <- generateGBMFullResults(trainRecs,testRecs,300)

model <- gbm(as.formula(paste0("G","~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+A9+B9+C9+D9+E9+F9+G9+costDir+costDir1")),
             data= trainAug, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=500, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot               
prob<-as.data.frame(predict(model,newdata=trainAug, 
                            type="response", 
                            n.trees=500)) 
outVar <- "G"
colnames(prob) <- levels(trainAug[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar

write.csv(pred_class,"trainG5.csv",row.names=FALSE, quote=FALSE)

trainAug$G5 <- pred_class$G

prob<-as.data.frame(predict(model,newdata=testAug, 
                            type="response", 
                            n.trees=500)) 
outVar <- "G"
colnames(prob) <- levels(testAug[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar
write.csv(pred_class,"testG5.csv",row.names=FALSE, quote=FALSE)

testAug$G5 <- pred_class$G


rm(model)
model <- gbm(as.formula(paste0("A","~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+A9+B9+C9+D9+E9+F9+G9+G5")),
             data= trainAug, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=500, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot               
prob<-as.data.frame(predict(model,newdata=testAug, 
                            type="response", 
                            n.trees=500)) 
outVar <- "A"
colnames(prob) <- levels(testAug[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar
testAug$A5 <- pred_class$A
write.csv(pred_class,"testA5.csv",row.names=FALSE, quote=FALSE)

predBoostOut <- subset(testAug, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predBoostOut$A1 <- testAug$A5
predBoostOut$G1 <- testAug$G5

predBoostOut <- collapsePreds(predBoostOut)

write.csv(predBoostOut,"predBoostOut.csv",row.names=FALSE, quote=FALSE)


##################################################################################################
#                          END : Collab Boosted for submission                                 #
##################################################################################################


##################################################################################################
#                          BEGIN : Cost Dir based G                                              #
##################################################################################################


rm(list=ls(all=TRUE))
setwd("~/work/kaggle/allstate/")
source("./AllStateFUNs.R")

#augmentTrainingData()
trainAug <- loadAugTrainingData()
testAug <- loadTestData()


# preds <- generateGBMFullResults(trainRecs,testRecs,300)
numTrees <- 1000
model <- gbm(as.formula(paste0("G","~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+costDir+costDir1")),
             data= trainAug, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=numTrees, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot               

prob<-as.data.frame(predict(model,newdata=testAug, 
                            type="response", 
                            n.trees=numTrees)) 
outVar <- "G"
colnames(prob) <- levels(testAug[,outVar])
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar

predBoostOut <- subset(testAug, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predBoostOut$G1 <- pred_class$G
predBoostOut <- collapsePreds(predBoostOut)

write.csv(gBoostOut,"predBoostOut05May.csv",row.names=FALSE, quote=FALSE)


cor(##################################################################################################
#                          END : Cost Dir based G                                                #
##################################################################################################

##################################################################################################
#                          BEGIN : Submission for AG Boost                                       #
##################################################################################################


rm(list=ls(all=TRUE))
setwd("C:/axs/work/kaggle/allstate/")
source("./AllStateFUNs.R")

# augmentTrainingData()
trainAug <- loadAugTrainingData()

trainAug$AG <- paste0(trainAug$A, trainAug$G)
trainAug$AG <- as.factor(trainAug$AG)

trainAug$AG1 <- paste0(trainAug$A1, trainAug$G1)
trainAug$AG1 <- as.factor(trainAug$AG1)

trainAug$AG2 <- paste0(trainAug$A2, trainAug$G2)
trainAug$AG2 <- as.factor(trainAug$AG2)

numTrees <- 500

outVar <- "AG"

model <- gbm(as.formula(paste0(outVar,"~", paste0(c("state+homeowner+car_value+risk_factor",
                                                    "married_couple+carAgeBin+C_previous+shopping_pt",
                                                    "costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2",
                                                    "costDir+costDir1+AG1+AG2"),collapse="+"))),
             data= trainAug, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=numTrees, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot 

testAug <- loadTestData()


testAug$AG1 <- paste0(testAug$A1, testAug$G1)
testAug$AG1 <- as.factor(testAug$AG1)

testAug$AG2 <- paste0(testAug$A2, testAug$G2)
testAug$AG2 <- as.factor(testAug$AG2)

prob<-as.data.frame(predict(model,newdata=testAug, 
                            type="response", 
                            n.trees=numTrees)) 
colnames(prob) <- levels(testAug$G1)
pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
colnames(pred_class) <- outVar

predComb <- subset(testAug, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predComb$A1 <- as.factor(substr(pred_class$AG,1,1))
levels(predComb$A1) <- levels(testAug$A1)
predComb$G1 <- as.factor(substr(pred_class$AG,2,2))
levels(predComb$G1) <- levels(testAug$G1)

predCombOut <- collapsePreds(predComb)
write.csv(predCombOut,"predBoostOut01May.csv",row.names=FALSE, quote=FALSE)


##################################################################################################
#                          END : Submission for AG Boost                                         #
##################################################################################################


rm(list=ls(all=TRUE))
setwd("C:/axs/work/kaggle/allstate/")
source("./AllStateFUNs.R")

# augmentTrainingData()
trainAug <- loadAugTrainingData()
testAug <- loadTestData()

addStateIndex <- function(data) {
  stateLevels <- levels(data$state)
  stateLevels <- cbind(stateLevels,c(1:length(stateLevels)))
  colnames(stateLevels) <- c("state", "sIndex")
  data <- merge(data,stateLevels,by.x="state",by.y="state")
  data$sIndex <- as.integer(data$sIndex)
  return(data)
}

trainAug <- addStateIndex(trainAug)
testAug <- addStateIndex(testAug)

library(randomForest)

#Testing the model using training data (70/30)
totalRows <- nrow(trainAug)
trainCustIds <- sample(unique(trainAug$customer_ID),size=0.7*totalRows,replace=F)
trainRecs <- trainAug[which(trainAug$customer_ID %in% trainCustIds),]
testRecs <- trainAug[which(!(trainAug$customer_ID %in% trainCustIds)),]

testAug$car_value <- factor(testAug$car_value)

outVar <- "G"
formula <- as.formula(paste0(outVar,"~", paste0(c("state+homeowner+car_value",
                                                  "married_couple+carAgeBin+C_previous+shopping_pt",
                                                  "costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2",
                                                  "costDir"),collapse="+")))
set.seed(1234)
numTrees <- 500
modelrf <- randomForest(formula,
                        data=trainRecs,
                        ntree=numTrees,
                        na.action=na.omit
             ) 

predG <- predict(modelrf,testRecs)

model <- gbm(as.formula(paste0(outVar,"~", "state+homeowner+car_value+risk_factor+married_couple+carAgeBin+C_previous+shopping_pt+costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+costDir+costDir1")),
#model <- gbm(formula,
             #cv.folds = 3,
             data= trainAug, # dataset
             distribution="multinomial", # see the help for other choices
             n.trees=numTrees, # number of trees)
             shrinkage=0.01, # shrinkage or learning rate,
             # 0.001 to 0.1 usually work
             interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
             bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
             n.minobsinnode = 10, # minimum total weight needed in each node
             verbose=FALSE) # don't print out progressplot 

plot(modelrf)

importance(modelrf)
at

predBoostOut <- subset(testRecs, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predBoostOut$G1 <- predG
predBoostOut$G1[which(is.na(predBoostOut))] <- testRecs$G1[which(is.na(predBoostOut))]
predBoostOut <- collapsePreds(predBoostOut)
actual <- collapsePreds(subset(testRecs, select=c("customer_ID", LETTERS[1:7])))


predG <- predict(modelrf,testAug)
predG[which(is.na(predG))] <- testAug$G1[which(is.na(predG))]


predBoostOut <- subset(testAug, select=c("customer_ID", paste0(LETTERS[1:7],1)))
predBoostOut$G1 <- predG

predBoostOut <- collapsePreds(predBoostOut)

write.csv(predBoostOut,"predBoostOut05May.csv",row.names=FALSE, quote=FALSE)


table(actual$plan == predBoostOut$plan)

dp <- discretize(trainAug$duration_previous, disc="equalwidth", nbins=15)
trainAug$duration_prev_disc <- dp$X


dp <- discretize(testAug$duration_previous, disc="equalwidth", nbins=15)
testAug$duration_prev_disc <- dp$X


numTrees <- 1500
 trainData <- trainAug
 testData <- testAug
# trainData <- trainRecs
# testData <- testRecs

trainData$GG1 <- as.factor(trainData$G == trainData$G1)

trainData <- trainRiskFactor
testData <- testRiskFactor

library(infotheo)
trainData$ageOldBin <- as.factor(discretize(trainData$age_oldest,disc="equalwidth", nbins="10")$X)
trainData$ageYngBin <- as.factor(discretize(trainData$age_youngest,disc="equalwidth", nbins="10")$X)

testData$ageOldBin <- as.factor(discretize(testData$age_oldest,disc="equalwidth", nbins="10")$X)
testData$ageYngBin <- as.factor(discretize(testData$age_youngest,disc="equalwidth", nbins="10")$X)


#outVars <- c("A", "B", "C", "D", "E", "F", "G")
outVars <- c("D")

predOut <- data.frame(testData$customer_ID)
model <- NULL
for (outVar in outVars) {
  
  print(paste0("Training for: ", outVar))
  
  rm(model)
  model <- gbm(as.formula(paste0(outVar,"~", 
               paste0(c("state+homeowner+car_value+duration_previous", "day",
                                    "risk_factor", "ageOldBin", "ageYngBin",
                                    "married_couple+carAgeBin+C_previous+shopping_pt",
                                    "costBin+A1+B1+C1+D1+E1+F1+G1+A2+B2+C2+D2+E2+F2+G2+A9+B9+C9+D9+E9+F9+G9",
                                    "costDir+costDir1"),collapse="+"))),
               data= trainData, # dataset
               distribution="multinomial", # see the help for other choices
               n.trees=numTrees, # number of trees)
               shrinkage=0.01, # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=5, # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
               n.minobsinnode = 10, # minimum total weight needed in each node
               verbose=FALSE) # don't print out progressplot 
  
  prob<-as.data.frame(predict(model,newdata=testData, 
                              type="response", 
                              n.trees=numTrees)) 
  
  colnames(prob) <- levels(trainData[,outVar])
  pred_class <- data.frame(apply(prob,1,function(x) return(names(which.max(x))[1])))
  colnames(pred_class) <- outVar
  
  predOut <- cbind(predOut,pred_class[,outVar])
  
}
colnames(predOut) <- c("customer_ID", outVars)

predBoostOut <- read.csv("bestPredBoostOut.csv")

predBoostOut <- subset(testData, select=c("customer_ID", paste0(LETTERS[1:7],1)))

#  write.csv(predBoostOut, "bestPredBoostOut.csv", row.names=FALSE)

predBoostOut$G1 <- predOut$G

predBoostOut$D1 <- predOut$D

predBoostOut$B1 <- predOut$B

predBoostOut <- collapsePreds(predBoostOut)


write.csv(predBoostOut,"predBoostOut17May4.csv",row.names=FALSE, quote=FALSE)


#####################################################################


nnReco <- function(x, outVar, inVars) {
  query <- paste("SELECT", outVar, ", COUNT(1) `cnt` FROM trainData WHERE ") 
  for (inVar in inVars) {
    query <- paste0(query,inVar,"='",x[,inVar], "'")
    query <- paste(query, "AND ")
  }
  query <- paste(query, "1=1 ORDER BY cnt DESC LIMIT 1 ")
  #print(query)
  res <- sqldf(query)
  #print(res)
}

testData <- testRecs
trainData <- trainRecs
predOut <- testRecs[,c("customer_ID", "G1")]
for (i in 1:nrow(testData)) {
  x <- testData[i,]
  #reco <- nnReco(x, "G", c("state","group_size","homeowner", "carAgeBin", "costBin", "risk_factor", "married_couple", "C_previous", "car_value", "A2","B2","C2","D2","E2","F2"))
  reco <- nnReco(x, "G", c("state","risk_factor", "married_couple", "A2","B2","C2","D2","E2","F2"))
  
  if (!is.na(reco$G)) {
    predOut$G2[i] <- reco$G
  }
}
x <- testRecs[1,]
nnReco(x, "G", c("state","group_size","homeowner", "carAgeBin", "costBin", "risk_factor", "married_couple", "C_previous", "car_value", "A2","B2","C2","D2","E2","F2"))






nnReco(x, "G", c("state","group_size","homeowner", "G2"))


##########################################################

# install.packages('neuralnet')
library("neuralnet")
#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library
#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)
#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)
#Plot the neural network
plot(net.sqrt)
#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network
#Lets see what properties net.sqrt has
ls(net.results)
#Lets see the results
print(net.results$net.result)
#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)

####################################################

selCols <- c("state","homeowner","car_value","duration_previous","risk_factor",
             "married_couple","carAgeBin","C_previous","shopping_pt",
             "costBin","A1","B1","C1","D1","E1","F1","G1","A2","B2","C2","D2","E2","F2","G2",
             "costDir","costDir1", "G")
selCols <- c("G","state", "G1", "G2")

m <- model.matrix(~G+state+G1+G2, data=trainRecs[,selCols])

paste0(colnames(m), collapse="+")

net.sqrt <- neuralnet(G2~stateAR+stateCO+stateCT+stateDC+stateDE+stateFL+stateGA+stateIA+stateID+stateIN+stateKS+stateKY+stateMD+stateME+stateMO+stateMS+stateMT+stateND+stateNE+stateNH+stateNM+stateNV+stateNY+stateOH+stateOK+stateOR+statePA+stateRI+stateSD+stateTN+stateUT+stateWA+stateWI+stateWV+stateWY+G12+G13+G14+G22+G23+G24,
                      data=m, hidden=20, threshold=0.01)


head(trainRecs[,selCols])
class(trainRecs[,selCols])


