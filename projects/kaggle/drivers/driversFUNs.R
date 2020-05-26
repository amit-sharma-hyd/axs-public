addFeatures <- function(trip)
{
  #   vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  
  sampleRate <- 1
  vitesse =  c(0, round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  accl = c(0, diff(vitesse,1,1)) 
  
  sampleRate <- 2
  vitesse2 =  c(0, round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  accl2 = c(0, diff(vitesse2,1,1)) 
  
  sampleRate <- 3
  vitesse3 =  c(0, round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  accl3 = c(0, diff(vitesse3,1,1)) 
  
  sampleRate <- 4
  vitesse4 =  c(0, round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  accl4 = c(0, diff(vitesse4,1,1)) 
  
  
  sampleRate <- 5
  vitesse5 =  c(0, round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  accl5 = c(0, diff(vitesse5,1,1)) 
  
  #accl = c(0, round(diff(vitesse,1,1), digits=3))
  smoothAccl1 <- c(rep(0,2), diff(vitesse,2,1))
  smoothAccl2 <- c(rep(0,3), diff(vitesse,3,1))
  
  qBreaks = c(seq(0.1,0.8, by = 0.2), seq(0.8,1,by=0.05))
  #qBreaks = c(seq(0.05,0.85, by = 0.1), seq(0.5,0.95,by=0.05))
  qBreaks <- unique(qBreaks)
  qBreaks <- qBreaks[qBreaks!=0]
  
  # Calculate smoothened speed and acceleration
  sampleRate <- 10
  smoothVitesse = c(rep(0,sampleRate), round(3.6*sqrt(diff(trip$x,sampleRate,1)^2 + diff(trip$y,sampleRate,1)^2)/sampleRate, digits=3))
  #   smoothAcc3 = diff(smoothVitesse,1,1)
  
  # Calculate the end-to-end displacement
  tripRadialDistance <- round(sqrt((trip[nrow(trip),]$x-trip[1,]$x)^2 + (trip[nrow(trip),]$y-trip[1,]$y)^2), digits=3)
  
  # Overall average speed  
  tripAvgSpeed <- tripRadialDistance/nrow(trip)
  
  # Calculate the road distance
  trip5thSec <- trip[(as.numeric(row.names(trip)))%%5==0,]
  tripRoadDistance <- round(sum(sqrt(diff(trip5thSec$x,1,1)^2 + diff(trip5thSec$y,1,1)^2)), digits=3)
  
  #look for bends
  sFreq <- 1
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(rep(0,sFreq), sqrt(diff(tripZ$x,sFreq,1)^2 + diff(tripZ$y,sFreq,1)^2))
  skipDistSampled <- c(0,0,sqrt(diff(tripZ$x,sFreq*2,1)^2 + diff(tripZ$y,sFreq*2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist1 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed1 =  round(bendDist1*bSpeed, digits=3)
  bendAccl1 = round(bendDist1*bAccl,digits=3)
  
  sFreq = 2
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(0, sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2))
  skipDistSampled <- c(0, 0, sqrt(diff(tripZ$x,2,1)^2 + diff(tripZ$y,2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist2 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed2 =  round(bendDist2*bSpeed, digits=3)
  bendAccl2 = round(bendDist2*bAccl,digits=3)
  
  sFreq = 3
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(0, sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2))
  skipDistSampled <- c(0, 0, sqrt(diff(tripZ$x,2,1)^2 + diff(tripZ$y,2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist3 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed3 =  round(bendDist3*bSpeed, digits=3)
  bendAccl3 = round(bendDist3*bAccl,digits=3)
  
  sFreq = 4
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(0, sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2))
  skipDistSampled <- c(0, 0, sqrt(diff(tripZ$x,2,1)^2 + diff(tripZ$y,2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist4 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed4 =  round(bendDist4*bSpeed, digits=3)
  bendAccl4 = round(bendDist4*bAccl,digits=3)
  
  sFreq = 5
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(0, sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2))
  skipDistSampled <- c(0, 0, sqrt(diff(tripZ$x,2,1)^2 + diff(tripZ$y,2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist5 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed5 =  round(bendDist5*bSpeed, digits=3)
  bendAccl5= round(bendDist5*bAccl,digits=3)
  
  sFreq = 10
  tripZ <- trip[(as.numeric(row.names(trip)))%%sFreq==0,]
  distanceSampled <- c(0, sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2))
  skipDistSampled <- c(0, 0, sqrt(diff(tripZ$x,2,1)^2 + diff(tripZ$y,2,1)^2))
  sumDistanceSampledPairs <- c(0, distanceSampled[2:length(distanceSampled)] + distanceSampled[1:(length(distanceSampled)-1)])
  bendDist10 <- round(sumDistanceSampledPairs - skipDistSampled, digits=3)
  bSpeed <- c(0, round(sqrt(diff(tripZ$x,1,1)^2 + diff(tripZ$y,1,1)^2),digits=3))
  bAccl <- c(0,round(diff(bSpeed,1,1),digits=3))
  bendSpeed10 =  round(bendDist10*bSpeed, digits=3)
  bendAccl10 = round(bendDist10*bAccl,digits=3)
  
  bendSpeed1Quantiles <- quantile(bendSpeed1, qBreaks)
  names(bendSpeed1Quantiles) <- paste0("BendSpeed1Q",1:length(bendSpeed1Quantiles))
  
  bendSpeed2Quantiles <- quantile(bendSpeed2, qBreaks)
  names(bendSpeed2Quantiles) <- paste0("BendSpeed2Q",1:length(bendSpeed2Quantiles))
  
  bendSpeed3Quantiles <- quantile(bendSpeed3, qBreaks)
  names(bendSpeed3Quantiles) <- paste0("BendSpeed3Q",1:length(bendSpeed3Quantiles))
  
  bendSpeed4Quantiles <- quantile(bendSpeed4, qBreaks)
  names(bendSpeed4Quantiles) <- paste0("bendSpeed4Q",1:length(bendSpeed4Quantiles))
  
  bendSpeed5Quantiles <- quantile(bendSpeed5, qBreaks)
  names(bendSpeed5Quantiles) <- paste0("bendSpeed5Q",1:length(bendSpeed5Quantiles))
  
  bendSpeed10Quantiles <- quantile(bendSpeed10, qBreaks)
  names(bendSpeed10Quantiles) <- paste0("bendSpeed10Q",1:length(bendSpeed10Quantiles))
  
  bendAccl1Quantiles <- quantile(bendAccl1, qBreaks)
  names(bendAccl1Quantiles) <- paste0("BendAccl1Q",1:length(bendAccl1Quantiles))
  
  bendAccl2Quantiles <- quantile(bendAccl2, qBreaks)
  names(bendAccl2Quantiles) <- paste0("bendAccl2Q",1:length(bendAccl2Quantiles))
  
  bendAccl3Quantiles <- quantile(bendAccl3, qBreaks)
  names(bendAccl3Quantiles) <- paste0("bendAccl3Q",1:length(bendAccl3Quantiles))
  
  bendAccl4Quantiles <- quantile(bendAccl4, qBreaks)
  names(bendAccl4Quantiles) <- paste0("bendAccl4Q",1:length(bendAccl4Quantiles))
  
  bendAccl5Quantiles <- quantile(bendAccl5, qBreaks)
  names(bendAccl5Quantiles) <- paste0("bendAccl5Q",1:length(bendAccl5Quantiles))
  
  bendDist1Quantiles <- quantile(bendDist1, qBreaks)
  names(bendDist1Quantiles) <- paste0("BendDistance1Q",1:length(bendDist1Quantiles))  
  
  bendDist2Quantiles <- quantile(bendDist2, qBreaks)
  names(bendDist2Quantiles) <- paste0("BendDistance2Q",1:length(bendDist2Quantiles))  
  
  bendDist3Quantiles <- quantile(bendDist3, qBreaks)
  names(bendDist3Quantiles) <- paste0("BendDistance3Q",1:length(bendDist3Quantiles))  
  
  bendDist4Quantiles <- quantile(bendDist4, qBreaks)
  names(bendDist4Quantiles) <- paste0("BendDistance4Q",1:length(bendDist4Quantiles))  
  
  bendDist5Quantiles <- quantile(bendDist5, qBreaks)
  names(bendDist5Quantiles) <- paste0("BendDistance5Q",1:length(bendDist5Quantiles))  
  
  bendDist10Quantiles <- quantile(bendDist10, qBreaks)
  names(bendDist10Quantiles) <- paste0("BendDistance10Q",1:length(bendDist10Quantiles))  
  
  speed1Quantiles <- quantile(vitesse, qBreaks)
  names(speed1Quantiles) <- paste0("Speed1Q",1:length(speed1Quantiles))
  
  speed2Quantiles <- quantile(vitesse2, qBreaks)
  names(speed2Quantiles) <- paste0("Speed2Q",1:length(speed2Quantiles))
  speed3Quantiles <- quantile(vitesse3, qBreaks)
  names(speed3Quantiles) <- paste0("Speed3Q",1:length(speed3Quantiles))
  speed4Quantiles <- quantile(vitesse4, qBreaks)
  names(speed4Quantiles) <- paste0("Speed4Q",1:length(speed4Quantiles))
  speed5Quantiles <- quantile(vitesse5, qBreaks)
  names(speed5Quantiles) <- paste0("Speed5Q",1:length(speed5Quantiles))
  
  accl2Quantiles <- quantile(accl2, qBreaks)
  names(accl2Quantiles) <- paste0("Accl2Q",1:length(accl2Quantiles))
  accl3Quantiles <- quantile(accl3, qBreaks)
  names(accl3Quantiles) <- paste0("Accl3Q",1:length(accl3Quantiles))
  accl4Quantiles <- quantile(accl4, qBreaks)
  names(accl4Quantiles) <- paste0("Accl4Q",1:length(accl4Quantiles))
  accl5Quantiles <- quantile(accl5, qBreaks)
  names(accl5Quantiles) <- paste0("Accl5Q",1:length(accl5Quantiles))
  
  smoothSpeedQuantiles <- quantile(smoothVitesse, qBreaks)
  names(smoothSpeedQuantiles) <- paste0("SmoothSpeedQ",1:length(smoothSpeedQuantiles))
  
  vrle <- rle(vitesse)
  stopDuration <- vrle[[1]][which(vrle[[2]] <= 0.5)]
  numStops <- length(stopDuration)
  stopDurationQuantiles <- rep(0, length(qBreaks))
  if (length(stopDuration)>0) {
    stopDurationQuantiles <- quantile(stopDuration, qBreaks)
  }
  names(stopDurationQuantiles) <- paste0("StopDurQ",1:length(stopDurationQuantiles))
  
  risk <- (abs(vitesse) * abs(accl))/(abs(vitesse)+abs(accl)+0.01)
  riskQuantiles <- quantile(risk, qBreaks)
  names(riskQuantiles) <- paste0("RiskQ",1:length(riskQuantiles))
  
  acclQuantiles <- rep(0, length(qBreaks))
  if (length(accl > 0)) {
    acclQuantiles <- quantile(accl, qBreaks)
  }
  names(acclQuantiles) <- paste0("AcclQ",1:length(acclQuantiles))
  
  smoothAccl1Quantiles <- rep(0, length(qBreaks))
  if (length(smoothAccl1) > 0) {
    smoothAccl1Quantiles <- quantile(smoothAccl1, qBreaks)
  }
  names(smoothAccl1Quantiles) <- paste0("SmoothAccl1Q",1:length(smoothAccl1Quantiles))
  
  smoothAccl2Quantiles <- rep(0, length(qBreaks))
  if (length(smoothAccl2 > 0))  {
    smoothAccl2Quantiles <- quantile(smoothAccl2, qBreaks)
  }
  names(smoothAccl2Quantiles) <- paste0("SmoothAccl2Q",1:length(smoothAccl2Quantiles))
  
  tt1 <- trip[1:(nrow(trip)-1),]
  tt2 <- trip[2:nrow(trip),]
  grads <- (tt2$y-tt1$y+0.001)/(tt2$x-tt1$x+0.001)
  gradQuantiles <- quantile(grads,qBreaks)    
  names(gradQuantiles) <- paste0("GradQ",1:length(gradQuantiles))
  
  tripRadialQuantiles <- c()
  radialStep <- 0.2
  for (q in seq(0.1,1-radialStep,by=radialStep)) {
    currentRadialQuantile <- sqrt((trip[floor(nrow(trip)*(q+radialStep)),]$x-trip[floor(nrow(trip)*q),]$x)^2 + (trip[floor(nrow(trip)*(q+radialStep)),]$y-trip[floor(nrow(trip)*q),]$y)^2)
    tripRadialQuantiles <- append(tripRadialQuantiles, currentRadialQuantile)
  }
  names(tripRadialQuantiles) <- paste0("RadialQ", 1:length(tripRadialQuantiles))
  
  addlFeatures = c(tripRadialDistance, tripRoadDistance, tripAvgSpeed, nrow(trip), numStops, numStops/tripRoadDistance)
  names(addlFeatures) = c("tripRadialDistance", "tripRoadDistance", "tripAvgSpeed", "tripTime", "numStops", "stopFreq")
  
  features <- c(speed1Quantiles, speed2Quantiles, speed3Quantiles, 
                speed4Quantiles, speed5Quantiles, smoothSpeedQuantiles, riskQuantiles,
                stopDurationQuantiles, acclQuantiles, smoothAccl1Quantiles,
                accl2Quantiles, accl3Quantiles, accl4Quantiles, accl5Quantiles,
                smoothAccl2Quantiles, tripRadialQuantiles, gradQuantiles, 
                bendSpeed1Quantiles, bendSpeed2Quantiles, bendSpeed3Quantiles,  
                bendSpeed4Quantiles, bendSpeed5Quantiles, bendSpeed10Quantiles,
                bendDist1Quantiles, bendDist2Quantiles, bendDist3Quantiles, 
                bendDist4Quantiles, bendDist5Quantiles, bendDist10Quantiles,
                bendAccl1Quantiles, bendAccl2Quantiles, 
                bendAccl3Quantiles, bendAccl4Quantiles, bendAccl5Quantiles, 
                addlFeatures)
  
  return(features)
}


binFeatures <- function(data, cols) {
  nbins <- 20
  for (col in cols) {
    data[,col] <- cut(data[,col], nbins, labels=LETTERS[1:nbins])
  }
  return(data)
}


# Returns the euclidean distance between two points
euc.dist <- function(x1, x2) sqrt(rowSums((x1 - x2) ^ 2))

#Scans through all the trip files for a specified driver and adds features
fetchTripData <- function(driverID, tripId) {
  fileName <- paste0("data/",driverID,"/",tripId,".csv")
  trip <- read.csv(fileName)
  trip[3] = 0
  trip[3][2:nrow(trip),] = trip[1][1:nrow(trip)-1,]
  trip[4] = 0
  trip[4][2:nrow(trip),] = trip[2][1:nrow(trip)-1,]
  trip[5] = apply(trip, 1, euc.dist)  
  trip[6]=0
  trip[6][2:nrow(trip),] = trip[5][1:nrow(trip)-1,]
  trip[7]=trip[5]-trip[6]
  colnames(trip) <- c("x","y","prevX","prevY","speed", "prevSpeed", "accl")
  return (trip)
}

processDriverData <- function(driverID) {
  driver <- data.frame(driverID=c(1:200))
  for (i in 1:200) {
    trip <- fetchTripData(driverID, i)
    # Speed Summary Cols
    driver[i,"speedQ1"] <- quantile(trip$speed, 0.25)[[1]]
    driver[i,"speedMedian"] <- quantile(trip$speed, 0.5)[[1]]
    driver[i,"speedQ3"] <- quantile(trip$speed, 0.75)[[1]]
    
    #Accl Summary Cols
    driver[i,"acclQ1"] <- quantile(trip$accl, 0.25)[[1]]
    driver[i,"acclMedian"] <- quantile(trip$accl, 0.5)[[1]]
    driver[i,"acclQ3"] <- quantile(trip$accl, 0.75)[[1]]
    
    #Overall end-to-end Distance
    driver[i,"totalDist"] <- sum(trip$speed)
    nrows <- nrow(trip)
    driver[i, "totalDisp"] <- sqrt((trip$x[1]-trip$x[nrows])^2+(trip$y[1]-trip$y[nrows])^2)
  }
  return (driver)
}

# Creates a summary file for each driver across trips
createTripSummary <- function() {
  driverDirs <- list.files("./data")
  results <- data.frame(driver_trip=paste0(rep(driverDirs,each=200),"_",rep(1:200, length(driverDirs))),prob=1)
  
  for (driverId in driverDirs) {
    print(paste("Now summarizing....",driverId))
    tripSummary <- processDriverData(driverId)
    write.csv(tripSummary, "./data/driverId/tripSummary.csv", quote=F, row.names=F)
    print(paste(driverId, "....Completed!"))
  }
  
}

# Returns the trips which do not correlate with majority of the specified driver trips
findOddTrips <- function(driverId) {
  driver <- processDriverData(driverId)
  speedCutOffLo <- quantile(driver$speedMedian,0.05)[[1]]
  speedCutOffHi <- quantile(driver$speedMedian,0.95)[[1]]
  
  outLiers <- which(driver$speedMedian<speedCutOffLo | driver$speedMedian>speedCutOffHi)
  return (outLiers)
}


plotDriverData <- function(driverId, beginPlot, numPlots) {
  driver <- data.frame(driverID=c(1:200))
  for (i in beginPlot:(beginPlot+numPlots)) {
    print (i)
    fileName <- paste0("data/",driverId,"/",i,".csv")
    trip <- read.csv(fileName)
    if (i==beginPlot) {
      plot(trip, type="l") } else {lines(trip, col=i)}
  }
}

plotSpeed <- function(driverId, tripIds) {
  for (tripId in tripIds){
    trip <- fetchTripData(driverId, tripId)
    if (tripId == 1) {
      plot(trip$speed, type='l') } else {
        line(trip$speed)
      }
  }
}

impFeatures <- function(rfModel,n) {
  ii <- importance(rfModel)
  return (ii[order(ii,decreasing=T)[1:n],])
}

plotDriverDataWithLabels <- function(trip, rng, dist, sumDist) {
  plot.new()
  pData = trip[rng,]
  lims <- c(min(c(pData$x,pData$x)), max(c(pData$y,pData$y)))
  plot(pData,type='b',xlim=lims, ylim=lims)
  text(pData,labels=rng,pos=1,col="green")
  text(pData,labels=round(dist[rng], digits=3),pos=2,col="red")
  text(pData,labels=round(sumDist[rng], digits=3),pos=3,col="blue")
}

getImpFeatures <- function(train,n) {
  train <- train[!duplicated(train[,1:(ncol(train)-1)]),]
  g = randomForest(target ~ ., 
                   data=train, ntree=500)
  ii <- importance(g)
  return (names(ii[order(ii,decreasing=T)[1:n],]))
}