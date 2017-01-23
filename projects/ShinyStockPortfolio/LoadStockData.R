file.path <- "/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio/Stocks/"
file.names <- c("Asian Paints.csv",  "Bharati Airtel.csv",  "BHEL.csv",  "Cipla.csv",  
                "Coal INDIA Ltd.csv",  "DLF.csv",  "Dr. Reddy's.csv",  "GAIL.csv",  
                "HCL TECH.csv",  "HDFC Bank.csv",  "Hero Motor Corp Ltd.csv",  "ICICI Bank.csv",  
                "ITC.csv",  "Jindal Steel.csv",  "LT.csv",  "Mahindra & Mahindra.csv",  
                "Maruti Suzuki.csv",  "ONGC.csv",  "Punjab National Bank.csv",  "Ranbaxy.csv",  
                "SBI.csv", "Tata Motors.csv",  "Tata Steel.csv",  "TCS.csv",  
                "Ultra Tech Cements.csv")

sel.stocks <- file.names[sample(length(file.names), 5, replace=FALSE)] 

#Calculates the daily returns for specified stock data
calcDayReturn <- function(stockData) {
  numRec <- nrow(stockData)
  stockData$Day.Return[1] <- 0
  for (i in 2:numRec) {
    stockData$Day.Return[i] <- 
      (stockData$Close.Price[i] - 
         stockData$Close.Price[i-1])*100/stockData$Close.Price[i-1]
  }
  return(stockData)
}

stock.data <- NULL
file.count <- 1
stocks <- c()
for (file.name in sel.stocks) {
  file.name <- paste(file.path, file.name, sep="")
  #Load data from file
  stock.data <- read.csv(file.name)
  #Format date column
  stock.data$Date <- as.Date(stock.data$Date, format="%d-%b-%y") 
  #Sort data based on Date col
  stock.data <- stock.data[order(stock.data$Date, decreasing=FALSE),]
  #Extract training data - 1 year data
  #stock.data <- stock.data[1:207,]
  #Add a column to store day returns
  stock.data <- calcDayReturn(stock.data)
  #Add data to my.stocks() list  
  stocks[file.count] <- list(stock.data)
  file.count <- file.count+1
}

returns.matrix <- NULL
#Create correlation matrix
for (stock in stocks) {
  returns.matrix <- cbind(returns.matrix, stock$Day.Return)
}
cor.matrix <- cor(returns.matrix)

getStats <- function() {
  stats <- data.frame(Mean=numeric(), Median=numeric(), SD=numeric())
  i <- 1
  for (stock in stocks) {
    stats[i,]$Mean <- mean(stock$Day.Return)
    stats[i,]$SD  <- sd(stock$Day.Return)
    stats[i,]$Median  <- median(stock$Day.Return)
    i <- i+1
  }
  rownames(stats) <- sel.stocks
  write.csv(stats,"StockReturns.csv")
  
  return(stats)
}

#Caculate Stocks Stas
stock.stats <- getStats()

#Remove all objects except selected stock names, stats and correlations
rm(list=setdiff(ls(),c("stock.stats", "cor.matrix", "stocks", "num.stocks", "sel.stocks")))
