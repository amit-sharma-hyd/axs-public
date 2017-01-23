library(shiny)

rm(list=ls(all=TRUE))


shinyServer(function(input, output) {
  
  
  #Number of simulations - both for stock data and number of distributions
  stock.sim.count <- 1000
  dist.sim.count <- 10000
  
  #Directory where the stock data files are stored
  file.path <- "/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio"
  
  #Names of stocks selected by user
  my.file.names <- reactive(getFileNames())  
  #Number of stocks selected by user
  num.stocks <- reactive(return(length(my.file.names())))
  
  my.stocks <- reactive(processStockFileData())
  my.stocks.stats <- reactive(getStats())
  my.profile.matrix <- reactive(getProfileMatrix())
  my.pfolio.returns <- reactive(getPfolioReturns())
  
  #Simulated Stock Daily Return Data using their means and stdev
  my.sim.data <- reactive(getSimData())
  
  #Simulated distributions (allocations) across selected stocks
  my.sim.dist.matrix <- reactive(getSimDistMatrix())
  
  #Product of my.sim.data and my.sim.dist.matrix
  my.stock.ret.sim.data <- reactive(getStockRetSimData())
  
  
  
  getIndPlotHeight <- function() {
    num.plot.rows <- ceiling(length(my.file.names())/3)
    return (num.plot.rows*350)
  }
  
  checkStocks <- renderPrint({
    if (is.na(input)) {
      return(NULL)
    }
    
    #List stock names and stats
    stats <- my.stocks.stats()
    #print(stats)
    
    returns.matrix <- NULL
    #Create correlation matrix
    for (stock in my.stocks()) {
      returns.matrix <- cbind(returns.matrix, stock$Day.Return)
    }
    
    #Check correlation
    if (length(my.stocks()) >1) {
      dimnames(returns.matrix) <- list(NULL, my.file.names())
      cor.matrix <- cor(returns.matrix)
      large.corel <- length(which(cor.matrix > 0.3))
      if (large.corel>length(my.file.names())) {
        cat("\nNot a good choice...\n\n")
        hi.corel <- which(cor.matrix > 0.3, arr.ind=TRUE) 
        len <- length(hi.corel[,1])
        i <- 1
        for (i in 1:len) {
          cor.row <- hi.corel[i,1]
          cor.col <- hi.corel[i,2]
          if (cor.row < cor.col) {
            cat(my.file.names()[cor.row], " has hi-correlation (",
                cor.matrix[cor.row, cor.col],") with ", 
                my.file.names()[cor.col], "\n", sep="")
          } 
        }
      }
    }
  })
  
  getFileNames <- function() {
    return (input$stocks)    
  }
  
  getStats <- function() {
    file.names <- my.file.names()
    stats <- data.frame(Mean=numeric(), Median=numeric(), SD=numeric())
    i <- 1
    for (stock in my.stocks()) {
      stats[i,]$Mean <- mean(stock$Day.Return)
      stats[i,]$SD  <- sd(stock$Day.Return)
      stats[i,]$Median  <- median(stock$Day.Return)
      i <- i+1
    }
    rownames(stats) <- my.file.names()
    return(stats)
  }
  
  getSimData <- function() {
    simStockReturns <- matrix(nrow=stock.sim.count, ncol=length(my.file.names()))
    
    i <- 1
    for (stock in my.stocks()) {
      mean <- my.stocks.stats()$Mean[i]
      median <- my.stocks.stats()$Median[i]
      sdev <- my.stocks.stats()$SD[i]
      simStockReturns[,i] <- rnorm(stock.sim.count, mean, sdev)
      i <- i+1
    }
    return(simStockReturns)
  }
  
  processStockFileData <- function() {
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
    for (file.name in my.file.names()) {
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
    return(stocks)
  }
  
  #Plot charts
  myColors <- c("slateblue3", "red", "violetred1", "darkgoldenrod", "steelblue3", "darkseagreen3",
                "dodgerblue2", "darksalmon")
  
  ########### INDIVIDUAL PLOTS OF DAILY RETURNS ###########
  output$indivPlots <- renderPlot ({
    num.img <- length(my.file.names())
    if (num.img>0) {
      plotDayReturn <- function(dayReturnVector, chartTitle, lineColor) {
        hist(dayReturnVector, main=chartTitle, col=lineColor)
      }
      
      i <- 1
      num.img.rows <- ceiling(num.img/3)
      par(mfrow=c(num.img.rows,3),cex.main=1.5, cex.axis=1.5)
      for (my.stock in my.stocks()) {
        plotDayReturn(my.stock$Day.Return, my.file.names()[i], my.colors.light[i])
        i <- i+1
      }
    }
  }, height=getIndPlotHeight)
  
  my.colors.light <- c("lightpink", "lightsalmon", "lightskyblue3",  "lightgoldenrod1", 
                       "lightseagreen", "lightcoral", "lightgoldenrod1" )
  
  output$returnsPlot <- renderPlot ({
    plot.new()
    num.plot <- length(my.file.names())
    
    if (num.plot>0) {
      plot.names <- c(substr(my.file.names(), 1, 5), "Portfolio")
      
      #Plot settings, Font size 1.25x times
      par(mfrow=c(2,2),cex.main=1.2, cex.axis=1.2)
      
      plot.data <- my.stocks.stats()
      pfolio.returns.mean <- mean(my.pfolio.returns())
      pfolio.returns.median <- median(my.pfolio.returns())
      pfolio.returns.sdev <- sd(my.pfolio.returns())
      plot.data <- rbind(plot.data, c(pfolio.returns.mean, 
                                      pfolio.returns.median, pfolio.returns.sdev))
      
      #Plot the Return for each stock and the overall return
      x.labels <- paste(plot.names, "\n(", round(plot.data$Mean,digits=2), "%)", sep="")
      barplot(plot.data$Mean, axes=TRUE, names.arg=x.labels, 
              col=my.colors.light, main="Daily Returns")
      abline(a=0,b=0)
      #box(col="lightgoldenrod2")
      
      #Plot the SD for each stock and overall SD
      #par(mar=c(0,3,5,1))
      x.labels <- paste(plot.names, "\n(", round(plot.data$SD,digits=2), ")", sep="")
      barplot(plot.data$SD, axes=TRUE, names.arg=x.labels, 
              col=my.colors.light, main="Risk")
      
      
      #par(mfrow=c(1,2),cex.main=1, cex.axis=1)
      
      #Plot the percentage allocation of money to each stock
      dist <- c(my.profile.matrix())*100
      x.labels <- paste(c(substr(my.file.names(), 1, 5)),
                        "\n(", round(dist,digits=2), "%)", sep="")
      #Need to add an empty bar for alignment with other charts
      dist <- c(dist,0)
      x.labels <- c(x.labels, "")
      barplot(dist, axes=TRUE, names.arg=x.labels,
              col=my.colors.light, main="Investment Allocation") 
      
      #This plot shows returns vs risk when the selected profile is "Highest Returns"
      #Plot Risk Vs Returns Scatter
      plot.data <- my.stock.ret.sim.data()
      if (input$profile == "hi.ret") {
        plot(apply(plot.data,2, mean), apply(plot.data, 2, sd), type="point", 
           main="Risk Vs Return Simulation", xlab="Returns", ylab="Risk" )
    
      }
    }
  })
  
  getSimDistMatrix <- function() {
    #Create a set of simulated distributions which we shall use to identify the best from
    sim.dist.matrix <- matrix(runif(num.stocks()*dist.sim.count), nrow=dist.sim.count)
    sim.dist.matrix <- sim.dist.matrix/rowSums(sim.dist.matrix)
    
    return (sim.dist.matrix)
  }
  
  getStockRetSimData <- function() {
    stock.means <- matrix(my.stocks.stats()$Mean)
    
    #Start with allocating all money to a single stock
    num.stocks <- length(my.file.names())
    
    #write.csv(sim.dist.matrix, "simDistMatrix.csv")
    stock.ret.matrix <-  my.sim.data()  %*% t(my.sim.dist.matrix())
    
    return (stock.ret.matrix)
  }
  
  getHiRetMatrix <- function() {
    stock.ret.matrix <- my.stock.ret.sim.data()
    
    #Identify the distribution which gets maximum returns for a given risk
    avg.ret <- apply(stock.ret.matrix,2,mean)
    sd.ret <- apply(stock.ret.matrix,2,sd)
    
    eval.df <- data.frame(avg.ret)
    eval.df <- cbind(eval.df, sd.ret)
    
    sd.within.limits <- which(eval.df$sd.ret<input$risk.limit)
    if (length(sd.within.limits > 0)) {
      max.index <- which(eval.df$avg.ret==max(eval.df[which(eval.df$sd.ret<input$risk.limit),]$avg.ret))
    } else {
      max.index <- which(avg.ret==max(avg.ret))
    }
    
    sim.dist.matrix <- matrix(my.sim.dist.matrix(), ncol=num.stocks())
    
    ret.matrix <- matrix(sim.dist.matrix[max.index,], nrow=num.stocks())
    return(ret.matrix)
  }
  
  getProfileMatrix <- function() {
    num.stocks <- length(my.file.names())
    profile.matrix <- matrix(rep(0, num.stocks))
    
    if (input$profile == "equal") {
      profile.matrix <- matrix(rep(1/num.stocks, num.stocks), ncol=1)
    } else if (input$profile == "hi.ret") {
      profile.matrix <- getHiRetMatrix()
    } else if (input$profile == "weighted") {
      #Collect the mean of each selected stock  
      profile.matrix <- matrix(my.stocks.stats()$Mean, ncol=1)
      #Remove -ve returns and replace them with 0
      profile.matrix[profile.matrix<0] <- 0
      #Normalize so the ratio sum up to 1
      profile.matrix <- profile.matrix/sum(profile.matrix)
    }
    
    return (profile.matrix)
  }
  
  getPfolioReturns <- function() {
    returns <- my.sim.data() %*% my.profile.matrix()
    return(returns)
  }
  
  output$returnsTable <- renderTable({
    if (length(my.file.names() > 0)) {
      ret.table.data <- data.frame(my.stocks.stats())
      pfolio.data <- c(mean(my.pfolio.returns()), median(my.pfolio.returns()), sd(my.pfolio.returns()))
      
      ret.table.data <- rbind(ret.table.data, pfolio.data)
      rownames(ret.table.data) <- c(my.file.names(), "Portfolio")
      ret.table.data
    }
  })
  
  output$returnsDetails <- renderPrint ({
    num.stocks <- length(my.file.names())
    if (num.stocks > 0) {
      profile.matrix <- my.profile.matrix()
      
      result.returns <- my.pfolio.returns()
      
      result.returns.mean <- mean(result.returns)
      result.returns.sdev <- sd(result.returns)
      
      profile.matrix <- paste(100*profile.matrix, "%", sep="")
      
      cat(checkStocks())
      cat("\n\n")
      cat("Distribution recommended: ", profile.matrix, "\n")
      cat("\nPortfolio Returns: Mean:", result.returns.mean, ", Risk: ", result.returns.sdev, "\n")
      
    }    
  })
  
})
