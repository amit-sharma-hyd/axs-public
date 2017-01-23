# Problem: Using Genetic Algorithm, we need to identify maximum return that a
# stock portfolio can give for a specified risk constraint. This is a non-linear
# optimization problem since the constraint is quadratic.

# So, lets decide our genetic algorithm
# Our gene is going to be the weight of the stock
# Chromosome is going to be the the weight of each of the 5 stocks
# Mutation will happen 20% of the times, crossOver 80%

rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio")

# Load stock data for randomly selected num.stocks
source("LoadStockData.R")

#Remove all objects except selected stock names, stats and correlations
rm(list=setdiff(ls(),c("stock.stats", "cov.mat", "stocks", "num.stocks", "sel.stocks")))

source("genAlgFun.R")

# Mutation will shift the weights by 1 position and move 5th weight over to 1st
mutate<-function(individual){
  i <- sample(1:5,1)
  individual[i] <- 1 - individual[i]
  individual <- individual/sum(individual)
  return(individual)
}


crossOver=function(p1,p2){
  # bred <- (p1+p2)/2
  bred <- c(p1[[1]], p2[[2]], p1[[3]], p2[[4]], p1[[5]])
  bred <- bred/sum(bred)
  return(bred)
}

# Compares the returns/risk with a tolerable risk constraint
evalFitness<-function(individual){
  pfolio.risk <- sqrt(individual %*% cov.mat %*% individual)
  fitness=100
  if (pfolio.risk < risk.max.limit) {
    pfolio.ret <- individual %*% stock.stats$Mean
    fitness = -1*pfolio.ret
  }
  return(fitness)
}

# Select no of stocks
num.stocks <- 5

# Lets specify the risk constraint as risk.limit with a certain value
risk.max.limit <- 1.5

rndm.dist <- matrix(runif(10000), ncol=5)
rndm.dist.rowsum <- apply(rndm.dist, 1, sum)
rndm.dist <- rndm.dist/rndm.dist.rowsum

recco.dist <- geneticAlgo(population=rndm.dist, fitnessFun=evalFitness, mutate=mutate,
                          crossOver=crossOver, mutProb=0.1, elite=0.4, maxiterations=15)
recco.dist <- as.matrix(recco.dist[1:5])
max.ret <- recco.dist %*% stock.stats$Mean * 100
risk.val <- sqrt(recco.dist %*% cov.mat %*% t(recco.dist))
if (risk.val > risk.max.limit) {
  cat ("Sorry, could not find any returns for risk less than: ", risk.max.limit)
} else {
  cat ("For a risk limit of: ", risk.val, ", Max Return expected: ", max.ret, "%", "\n", sep="")
  colnames(recco.dist) <- sel.stocks
  cat("Recommended weight distribution: \n")
  recco.dist
}