rm(list=ls())
setwd("C:\\axs\\work\\kaggle\\drivers\\")
source("driversFUNs.R")

driverDirs <- list.files("./data")
results <- data.frame(driver_trip=paste0(rep(driverDirs,each=200),"_",rep(1:200, length(driverDirs))),prob=1)

for (driverId in driverDirs) {
  print(paste("Now processing....",driverId))
  outLiers <- findOddTrips(driverId)
  oDriverTrips <- paste0(driverId,"_",outLiers)
  results[which(results$driver_trip %in% oDriverTrips),]$prob = 0
  print(paste(driverId, "....Completed!"))
}

write.csv(results,"results.csv",row.names=F,quote=F)

processDriverData(2)


par(mfrow=c(4,5))
for(i in 11:30)
{
  trip = read.csv(paste0(dirPath, i, ".csv"))
  plot(trip, type='l')
}

