rm(list=ls())
gc()
library(readr)
library(xgboost)

set.seed(721) #seed bag1:8, then eta=0.06not0.04&nround125not250: bag2:64, bag3:6, bag4:88, bag5: 0.03-300-seed666
#bag6:16, train[1:80000,], val=train[80001:120000,], 0.06, 125  #bag7: 888,train[65000:145000,], val=train[1:40000,], 0.06, 125 
#bag8: 888,train[65000:145000,], val=train[1:40000,], 0.03, 300

#seed bag9:9999, 0.02,300,random

#bag10:425, bag11:718, bag12:719, bag13:720, bag14:721

setwd('/home/devel/axs/work/kaggle/spleaf-mkt/scripts')

cat("reading the train and test data\n")
train <- read_csv("../input/30K/trainX.csv")
test  <- read_csv("../input/30K/testX.csv")


train.unique.count=lapply(train, function(x) length(unique(x)))
train.unique.count_1=unlist(train.unique.count[unlist(train.unique.count)==1])
train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])
train.unique.count_2=train.unique.count_2[-which(names(train.unique.count_2)=='target')]

delete_const=names(train.unique.count_1)
delete_NA56=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145175))
delete_NA89=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145142))
delete_NA918=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==144313))

#VARS to delete
#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS
print(length(c(delete_const,delete_NA56,delete_NA89,delete_NA918)))

train=train[,!(names(train) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]
test=test[,!(names(test) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

# From manual data analysis
datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")
datecolumns = datecolumns[which(datecolumns %in% colnames(train))]

train_cropped <- train[datecolumns]
train_cc <- data.frame(apply(train_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  train[dc] <- NULL
  train[dc] <- train_cc[dc]
}

train_cc <- NULL
train_cropped <- NULL
gc()

test_cropped <- test[datecolumns]
test_cc <- data.frame(apply(test_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  test[dc] <- NULL
  test[dc] <- test_cc[dc]
}

test_cc <- NULL
test_cropped <- NULL
gc()


# safe target and put it at the end again
train_target <- train$target
train$target <- NULL
train$target <- train_target

feature.names <- names(train)[2:ncol(train)-1]
# names(train)  # 1934 variables

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

cat("sampling train to get around 8GB memory limitations\n")
#train <- train[sample(nrow(train), 120000),]
train <- train[sample(nrow(train), 20000),]
gc()

# h <- sample(nrow(train), 80000)
h <- sample(nrow(train), 4000)
val<-train[-h,]
gc()

train <-train[h,]
gc()

#for bag 6
#val=train[80001:120000,]
#train=train[1:80000,]
#gc()

#for bag 7
#val=train[1:40000,]
#train=train[65000:145000,]
#gc()

dtrain <- xgb.DMatrix(data.matrix(train[,feature.names]), label=train$target)

train=train[1:3,]
gc()


dval <- xgb.DMatrix(data.matrix(val[,feature.names]), label=val$target)
val=val[1:3,]
gc()

watchlist <- watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.01, # 0.06, #0.01,
                max_depth           = 8, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.8, # 0.7
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 100, #950, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    #early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)


dtrain=0
gc()

dval=0
gc()


submission <- data.frame(ID=test$ID)
submission$target <- NA 
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  submission[rows, "target"] <- predict(clf, data.matrix(test[rows,feature.names]))
  
}


#cat("saving the submission file\n")
#write_csv(submission, "xgb3.csv")


library(ROCR)
actual <- read.csv("../input/30K/testY.csv")
perf <- performance(prediction(submission$target, actual[,1]), "tpr", "fpr")
plot(perf)

as.numeric(performance(prediction(submission$target, actual[,1]), "auc")@y.values)

