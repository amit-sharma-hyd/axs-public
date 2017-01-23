rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio")
library(lpSolve)

#Define Risk Profiles in the order Lo, Med, Hi risk distr
profile.conservative <- c(0.5,0.3,0.2)
profile.moderate <- c(0.2,0.5,0.3)
profile.aggressive <- c(0.2,0.2,0.6)

risk.df <- rbind(profile.conservative,profile.moderate,profile.aggressive)
colnames(risk.df) <- c("Low", "Med", "High")
rownames(risk.df) <- c("Conservative", "Moderate", "Aggressive")
risk.df

#Read the returns data from returns file
stock.returns <- read.csv("StockReturns.csv")
colnames(stock.returns)[1] <- "Name"
stock.returns

#Select any 5 stocks
num.stocks <- 5
sel.stocks <- stock.returns[sample(1:nrow(stock.returns),num.stocks,replace=FALSE),]

#Order the data based on Risk
sel.stocks <- sel.stocks[order(sel.stocks$SD),]
sel.stocks

# Chart plot settings
par(mfrow=c(1,1),cex.main=1, cex.axis=1)

#Bin the Risk
library(infotheo)
risk.level <- discretize(sel.stocks$SD,disc="equalfreq", nbins=3)
colnames(risk.level) <- "RiskLevel"
sel.stocks$RiskLevel <- risk.level
sel.stocks

#Define constraints
w1 <- c(1,0,0,0,0) 
w2 <- c(0,1,0,0,0) 
w3 <- c(0,0,1,0,0) 
w4 <- c(0,0,0,1,0) 
w5 <- c(0,0,0,0,1) 
weights <- data.frame(cbind(w1,w2,w3,w4,w5))
sum.weights <- apply(weights,1,sum)

min.weight <- 0.05 # Minimum investment in each stock
#Low Risk
lo.risk <- apply(weights[which(sel.stocks$RiskLevel==1)],1,sum)
#Med Risk
med.risk <- apply(weights[which(sel.stocks$RiskLevel==2)],1,sum)
#Hi Risk
hi.risk <- apply(weights[which(sel.stocks$RiskLevel==3)],1,sum) 

i <- 1

#For each risk profile calculate the recommended dist
for (i in 1:nrow(risk.df)) {
  #Define objective function
  obj <- sel.stocks$Mean
  cons <- rbind(w1, w2, w3, w4, w5, sum.weights, lo.risk, med.risk, hi.risk)
  dir <- c(rep(">=", 5), "=",  "<=", "<=", "<=")
  rhs <- c(rep(min.weight,5), 1, risk.df[i,1], risk.df[i,2], risk.df[i,3])
  res <- lp("max", obj, cons, dir, rhs, compute.sens=0)
  if (sum(res$solution) == 0) {
    res
  } else {
    output <- cbind(sel.stocks,res$solution)
    cat("Invest as follows for risk profile: ", rownames(risk.df)[i], "\n")
    print(output)
    
    chart.title <- paste("Risk profile: ", rownames(risk.df)[i])
    bar.colors <- c("yellowgreen", "yellowgreen", "orange", "orange", "tomato3", "tomato3")
    barplot(res$solution, names=substr(sel.stocks$Name, 1, 6), main=chart.title, col=bar.colors)
  }
  i <- i + 1
}
