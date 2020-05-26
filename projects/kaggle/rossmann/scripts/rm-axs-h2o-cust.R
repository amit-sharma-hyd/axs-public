library(h2o)
library(plyr)
rm(list=ls())
setwd("/home/devel/axs/work/kaggle/rossmann/")

trainAll <- read.csv("./input/train.csv")
stores <- read.csv("./input/store.csv")

# Dummify promo months
for (mCol in substr(month.name, 1 , 3)) {
  mDF <- data.frame(rep(0, nrow(stores)))
  col <- paste0("Promo_",mCol)
  colnames(mDF) <- col
  stores <- cbind(stores, mDF)
  stores[grep(mCol, stores$PromoInterval), col] <- 1
}

str(trainAll)
trainAll$Store <- as.factor(trainAll$Store)
trainAll$DayOfWeek <- as.factor(trainAll$DayOfWeek)
trainAll$Date <- as.Date(trainAll$Date)
trainAll$Open <- as.factor(trainAll$Open)
trainAll$Promo <- as.factor(trainAll$Promo)
trainAll$SchoolHoliday <- as.factor(trainAll$SchoolHoliday)
#trainAll$Sales <- NULL

storeCustMean <- ddply(trainAll, .(Store), summarize, custMean=mean(Customers))
stores <- merge(stores, storeCustMean, by="Store")

# Merge train and stores data
trainAll <- merge(trainAll, stores, by="Store", all.x=T)

trainAll$CompetitionOpenSince <- 12*(2015-trainAll$CompetitionOpenSinceYear) + trainAll$CompetitionOpenSinceMonth
trainAll$Customers <- NULL

set.seed(123)

trainIds <- sample(1:nrow(trainAll), 0.25*nrow(trainAll))
train <- trainAll[-trainIds,]
test <- trainAll[trainIds,]

localH2O <- h2o.init(ip = "localhost", port = 54321)

train.hex <- as.h2o(train, destination_frame = "train.hex")
test.hex <- as.h2o(train, destination_frame = "test.hex")

# gbm <- h2o.gbm(training_frame = train.hex, 
#                x=setdiff(colnames(train.hex),c("Sales")), 
#                y=c("Sales"),
#               ntrees = 250)

gbm <- h2o.gbm(training_frame = train.hex, 
               x=setdiff(colnames(train.hex),c("Sales")), 
               y=c("Sales"),
               ntrees = 250)

#Apparently number of customers is not available in the real test data
#test.hex <- subset(test.hex, select=-c(Customers))
pred.hex <- h2o.predict(gbm, newdata=test.hex)

# truth <- test.hex[test.hex$Sales >0,]$Sales
# pred <- pred.hex[test.hex$Sales>0,]

truth <- test.hex[test.hex$Sales >0,]$Sales
pred <- pred.hex[test.hex$Sales>0,]


rmsep <- sqrt(sum(((truth-pred)/truth)^2)/length(truth))
rmsep # library(h2o)
library(plyr)
rm(list=ls())
setwd("/home/devel/axs/work/kaggle/rossmann/")

trainAll <- read.csv("./input/train.csv")
stores <- read.csv("./input/store.csv")

# Dummify promo months
for (mCol in substr(month.name, 1 , 3)) {
  mDF <- data.frame(rep(0, nrow(stores)))
  col <- paste0("Promo_",mCol)
  colnames(mDF) <- col
  stores <- cbind(stores, mDF)
  stores[grep(mCol, stores$PromoInterval), col] <- 1
}

str(trainAll)
trainAll$Store <- as.factor(trainAll$Store)
trainAll$DayOfWeek <- as.factor(trainAll$DayOfWeek)
trainAll$Date <- as.Date(trainAll$Date)
trainAll$Open <- as.factor(trainAll$Open)
trainAll$Promo <- as.factor(trainAll$Promo)
trainAll$SchoolHoliday <- as.factor(trainAll$SchoolHoliday)
#trainAll$Sales <- NULL


# Add month dimension
trainAll$Month <- format(trainAll$Date, "%m")
trainAll$Month <- as.factor(trainAll$Month)

storeCustMean <- ddply(trainAll, .(Store, Month), summarize, custMean=mean(Customers))
stores <- merge(stores, storeCustMean, by="Store")

# Merge train and stores data
trainAll <- merge(trainAll, stores, by=c("Store"), all.x=T)

trainAll$CompetitionOpenSince <- 12*(2015-trainAll$CompetitionOpenSinceYear) + trainAll$CompetitionOpenSinceMonth
trainAll$Customers <- NULL

set.seed(123)

trainIds <- sample(1:nrow(trainAll), 0.25*nrow(trainAll))
train <- trainAll[-trainIds,]
test <- trainAll[trainIds,]

localH2O <- h2o.init(ip = "localhost", port = 54321)

train.hex <- as.h2o(train, destination_frame = "train.hex")
test.hex <- as.h2o(train, destination_frame = "test.hex")

# gbm <- h2o.gbm(training_frame = train.hex, 
#                x=setdiff(colnames(train.hex),c("Sales")), 
#                y=c("Sales"),
#               ntrees = 250)

gbm <- h2o.gbm(training_frame = train.hex, 
               x=setdiff(colnames(train.hex),c("Sales")), 
               y=c("Sales"),
               ntrees = 250)

#Apparently number of customers is not available in the real test data
#test.hex <- subset(test.hex, select=-c(Customers))
pred.hex <- h2o.predict(gbm, newdata=test.hex)

# truth <- test.hex[test.hex$Sales >0,]$Sales
# pred <- pred.hex[test.hex$Sales>0,]

truth <- test.hex[test.hex$Sales >0,]$Sales
pred <- pred.hex[test.hex$Sales>0,]


rmsep <- sqrt(sum(((truth-pred)/truth)^2)/length(truth))
rmsep #0.2501047

