
###################################################
# Unsupervised learning using KNN
###################################################
#rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\drivers\\")
library(gbm)
library(randomForest)
addFeatures <- function(trip)
{
  sampleRate <- 1
  vitesse = 3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate
  
  accl = diff(vitesse,1,1)
  
  # Calculate the end-to-end displacement
  tripRadialDistance <- sqrt((trip[nrow(trip),]$x-trip[1,]$x)^2 + (trip[nrow(trip),]$y-trip[1,]$y)^2)
  
  # Overall average speed  
  tripAvgSpeed <- tripRadialDistance/nrow(trip)
  
  # Calculate the road distance
  trip5thSec <- trip[(as.numeric(row.names(trip)))%%5==0,]
  tripRoadDistance <- sum(sqrt(diff(trip5thSec$x,1,1)^2 + diff(trip5thSec$y,1,1)^2))
  
  # Calculate the 1st 30s and last 30s distance
  #   first30sDist <- sqrt((trip[15,]$x-trip[5,]$x)^2 + (trip[15,]$y-trip[5,]$y)^2)
  #   last30sDist <- sqrt((trip[nrow(trip-5),]$x-trip[nrow(trip)-15,]$x)^2 + (trip[nrow(trip-5),]$y-trip[nrow(trip)-15,]$y)^2)
  # first30sDist <- mean(vitesse[1:30])
  # last30sDist <- mean(vitesse[length(vitesse):(length(vitesse)-60)])
  # 
  #   tripGradient <- (trip[nrow(trip),]$y - trip[1,]$y)/(trip[nrow(trip),]$x - trip[1,]$x)
  
  # Road Distance at various milestones - did not help!
  #trip10thMin <- trip[(as.numeric(row.names(trip)))%%60==0,]
  #distanceEvery10thMinute <- sqrt((trip10thMin$x-trip[1,]$x)^2 + diff(trip10thMin$y-trip[1,]$y)^2)
  #features = c(quantile(vitesse, seq(0.1,1, by = 0.1)), quantile(accl, seq(0.1,1, by = 0.1)), tripRadialDistance, tripRoadDistance, tripAvgSpeed, quantile(distanceEvery10thMinute, seq(0,1, by = 0.2)))
  
  speedQuantiles <- quantile(vitesse, c(seq(0.1,0.8, by = 0.1), seq(0.8,1,by=0.05)))
  names(speedQuantiles) <- paste0("SpeedQ",1:length(speedQuantiles))
  acclQuantiles <- quantile(accl, c(seq(0.1,0.8, by = 0.1), seq(0.8,1,by=0.05)))
  names(acclQuantiles) <- paste0("AcclQ",1:length(acclQuantiles))
  
  #   speedAccl <- vitesse*accl
  #   speedAcclQuantiles <- quantile(speedAccl,seq(0.05,1,by=0.1))
  #   speedAcclQuantileNames <- paste0("SpeedAccl", 1:length(speedAcclQuantiles))
  
  tripRadialQuantiles <- c()
  for (q in seq(0.1,1,by=0.2)) {
    currentRadialQuantile <- sqrt((trip[1,]$x-trip[floor(nrow(trip)*q),]$x)^2 + (trip[1,]$y-trip[floor(nrow(trip)*q),]$y)^2)
    tripRadialQuantiles <- append(tripRadialQuantiles, currentRadialQuantile)
  }
  tripRadialQuantileNames <- paste0("RadialQ", 1:length(tripRadialQuantiles))
  
  #     gradQuantiles <- quantile(abs(tripGradient), c(0.05,0.10,0.2,seq(0.2,1,by=0.2)))
  #     names(gradQuantiles) <- paste0("GradQ", 1:length(gradQuantiles))
  
  #   speedQuantiles <- quantile(vitesse, seq(0,1, by = 0.2))
  #   names(speedQuantiles) <- paste0("SpeedQ",1:length(speedQuantiles))
  #   acclQuantiles <- quantile(accl, seq(0,1, by = 0.2))
  #   names(acclQuantiles) <- paste0("AcclQ",1:length(acclQuantiles))
  
  addlFeatures = c(tripRadialDistance, tripRoadDistance, tripAvgSpeed, nrow(trip), tripRadialQuantiles)
  names(addlFeatures) = c("tripRadialDistance", "tripRoadDistance", "tripAvgSpeed", "tripTime",tripRadialQuantileNames)
  
  #   addlFeatures = c(tripRadialDistance, tripRoadDistance, tripAvgSpeed, nrow(trip), first30sDist, last30sDist, tripRadialQuantiles, speedAcclQuantiles)
  #   names(addlFeatures) = c("tripRadialDistance", "tripRoadDistance", "tripAvgSpeed", "tripTime", "first30sDist", "last30sDist", tripRadialQuantileNames, speedAcclQuantileNames)
  #   
  #   addlFeatures = c(tripRadialDistance, tripRoadDistance, tripAvgSpeed, nrow(trip))
  #   names(addlFeatures) = c("tripRadialDistance", "tripRoadDistance", "tripAvgSpeed", "tripTime")
  #   
  
  features = c(speedQuantiles, acclQuantiles, addlFeatures)
  return(features)
}

binFeatures <- function(data, cols) {
  nbins <- 20
  for (col in cols) {
    data[,col] <- cut(data[,col], nbins, labels=LETTERS[1:nbins])
  }
  return(data)
}

euc.dist <- function(x1, x2) sqrt(rowSums((x1 - x2) ^ 2))

drivers = list.files("data")

submission = NULL
#imp <- data.frame(col=1:(ncol(refData)-1))
# for(driver in drivers)
for(driver in c("2983"))
{
  print(driver)
  dirPath = paste0("data/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    #features = c(addFeatures(trip), target)
    features = addFeatures(trip)
    currentData = rbind(currentData, features)
  }
  #Scale the data
  currentData <- apply(currentData,2,function(x) (x-min(x))/(max(x)-min(x)))
  #train = rbind(currentData, refData)
  train = as.data.frame(currentData)
  #train$target = as.factor(train$target)
  
  rownames(train) <- NULL
  medianDistance=c()
  for(t in 1:nrow(train))
  {
    currTrip <- train[t,]
    #pick 10 random trips
    randomTrips <- train[sample(1:nrow(train),10),]
    currTrip[2:10,]<-currTrip[1,]
    rownames(currTrip) <- NULL
    distance <- euc.dist(currTrip, randomTrips)
    medianDistance <- append(medianDistance, median(distance))
  }
  outLiers <- order(medianDistance)[190:200] #Take the Top 10 outliers
  # hist(medianDistance, labels=T)
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  #result = cbind(labels, p)
  outProb <- rep(1,200)
  outProb[outLiers] <- 0
  result= cbind(labels, outProb)
  submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "results.csv", row.names=F, quote=F)

#extract driverId column
#results$driverId <- apply(results, 1, function(x) return(strsplit(as.character(x[1]), "_")[[1]][1]))

#Scale the probs - did not help improve scores at all
# groupAgg <- sqldf("select driverId, min(prob) as lo, max(prob) as hi from results group by driverId")
# newResults <- sqldf("select res.driver_trip, (res.prob-agg.lo)/(agg.hi-agg.lo) as prob from results res, groupAgg agg where agg.driverId = res.driverId")
# write.csv(newResults, "newresults.csv", row.names=F, quote=F)



