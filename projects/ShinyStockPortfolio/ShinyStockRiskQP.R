rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio")

library(quadprog)

# Select no of stocks 
num.stocks <- 5
# Load stock data for randomly selected num.stocks
source("LoadStockData.R")

# Chart plot settings
par(mfrow=c(1,1),cex.main=1, cex.axis=1)

#Selected Stocks
sel.stocks
#Create asset returns which will be used by portfolio 
asset.returns <- data.frame()
for (i in 1:num.stocks) {
  day.returns <- stocks[[i]]$Day.Return
  if (i == 1) {
    asset.returns <- data.frame(day.returns)
  } else {
    asset.returns <- cbind(asset.returns,day.returns)
  }
  i <- i+1
}

# Function to evaluate the minimum risk given a required return
portfolio <- function(assetReturns, targetReturn)
{
  # Arguments:
  # assetReturns - multivariate data set of asset returns
  # target Return - the portfolios target return
  # 1 Create Portfolio Settings:
  nAssets = ncol(assetReturns)
  Dmat = cov(assetReturns)
  dvec = rep(0, times=nAssets)
  Amat = t(rbind(
    Return=colMeans(assetReturns),
    Budget=rep(1, nAssets),
    LongOnly=diag(nAssets)))
  bvec = c(
    Return=targetReturn,
    budget=1,
    LongOnly=rep(0, times=nAssets))
  meq = 2
  # 2 Optimize Weights:
  portfolio = solve.QP(Dmat, dvec, Amat, bvec, meq)
  weights = round(portfolio$solution, digits = 4)
  names(weights) = colnames(assetReturns)
  # Return Value:
  list(
    weights = 100*weights,
    risk = portfolio$value,
    return = targetReturn)
}

#Invoke the portfolio function for different returns starting
#at 1% and incrementing by 0.1%
ret.reqd <- 0.01
risk.returns <- data.frame()
# Loop until returns are 100% (very unlikely to reach) OR the QP fails
# which is more likely
while (ret.reqd < 1) {
  tryCatch(
    #Handle Errors - Error message expected
    pfolio <- portfolio(asset.returns,ret.reqd), error=function(e) break
  )
  risk.returns <- rbind(risk.returns, c(pfolio$risk, pfolio$return, pfolio$weights))
  #1% increments to required return
  ret.reqd <- ret.reqd+(0.001)
}
colnames(risk.returns) <- c("Risk", "Returns", paste(c(rep("Weight",5)), c(1:5), sep=""))
head(risk.returns,10)

#Find the return with minimum risk
risk.min <- min(risk.returns$Risk)
risk.min.record <- risk.returns[which(risk.returns$Risk==risk.min),]
attach(risk.min.record)

#Plot the returns to risk efficiency frontier
plot(risk.returns[,1:2], type='l', col="blue", main="Risk-Return Efficiency Frontier")
abline(v=risk.min,h=Returns,col="red",lty=2)

cat("Minimum Risk: ", risk.min, ", at Return: ", Returns)

#Recommended distribution for minimum risk
risk.min.dist <- c(Weight1, Weight2, Weight3, Weight4, Weight5) 
pie.labels <- paste(sel.stocks, "\n(", risk.min.dist, "%)", sep="")
pie(risk.min.dist, labels=pie.labels, main="Recommended distribution \nfor minimum risk")
