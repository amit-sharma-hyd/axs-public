imputeMedian <- function(df) {
  for (i in 1:ncol(df)) {
    if (length(which(is.na(df[,i]))) > 0) {
      medianVal <- median(df[,i], na.rm=T) 
      print(medianVal)
      df[which(is.na(df[,i])),i] <- medianVal
    }
  }
  return (df);
}

imputeUsingLinReg <- function(df) {
  dfCC <- df[complete.cases(df),]
  for (i in 1:ncol(df)) {
    if (length(which(is.na(df[,i]))) > 0) {
      formula <- as.formula(paste0('dfCC$', names(df)[i], '~.'))
      print(formula)
      model <- lm(formula, data=dfCC)
      naIdx <- which(is.na(df[,i]))
      dfTemp <- df[naIdx,]
      dfTemp[is.na(dfTemp)] <- 0
      df[naIdx,i] <- predict(model, newdata=dfTemp)
    }
  }
  return(df)
}

stratified = function(df, id, group, size, seed="NULL", ...) {
  #  USE: * Specify your data frame, ID variable (as column number),
  #   and grouping variable (as column number) as the first three
  #   arguments.
  #    * Decide on your sample size. For a sample proportional to
  #   the population, enter "size" as a decimal. For an equal number
  #   of samples from each group, enter "size" as a whole number.
  #    * Decide on if you want to use a seed or not. If not, leave
  #   blank or type "NULL" (with quotes).
  #
  #  Example 1: To sample 10% of each group from a data frame named
  #    "z", where the ID variable is the first variable, the
  #    grouping variable is the fourth variable, and the desired
  #    seed is "1", use:
  #
  #     > stratified(z, 1, 4, .1, 1)
  #
  #  Example 2: Run the same sample as above but without a seed:
  #
  #     > stratified(z, 1, 4, .1)
  #
  #  Example 3: To sample 5 from each group from a data frame named
  #    "z", where the ID variable is the first variable, the
  #    grouping variable is the third variable, and the desired
  #    seed is 2, use:
  #
  #     > stratified(z, 1, 3, 5, 2)
  #
  #  NOTE: Not tested on datasets with LOTS of groups or with HUGE
  #  differences in group sizes. Probably INCREDIBLY inefficient.
  k = unstack(data.frame(as.vector(df[id]), as.vector(df[group])))
  l = length(k)
  results = vector("list", l)
  if (seed == "NULL" & size < 1) {
    for (i in 1:length(k)) {
      N = k[[i]]
      n = round(length(N)*size)
      results[[i]] = list(sample(N, n, ...))
    }
  } else if (seed == "NULL" & size >= 1) {
    for (i in 1:length(k)) {
      N = k[[i]]
      results[[i]] = list(sample(N, size, ...))
    }
  } else if (size < 1) {
    for (i in 1:length(k)) {
      set.seed(seed)
      N = k[[i]]
      n = round(length(N)*size)
      results[[i]] = list(sample(N, n, ...))
    }
  } else if (size >= 1) {
    for (i in 1:length(k)) {
      set.seed(seed)
      N = k[[i]]
      results[[i]] = list(sample(N, size, ...))
    }
  }
  z = data.frame(c(unlist(results)))
  names(z) = names(df[id])
  w = merge(df, z)
  w[order(w[group]), ]
}

plotAcc <- function(start, iter, prob, actual) {
  bestTH <- 0
  bestAcc <- 0
  res <- c()
  for (i in 1:iter) {
    threshold <- start + (0.01*i);
    print(threshold)
    
    pred <- rep(0, length(prob))
    pred [which(prob>threshold)]<- 1
    
    acc <- length(which(pred == actual))
    acc <- acc*100/length(pred)
    if (acc > bestAcc) {
      bestAcc <- acc
      bestTH <- threshold
    }
    res <- append(res, acc)
  }
  plot(res, type="b")
  print(paste("Best Threshold: ", bestTH))
}



plotAMS <- function(start, iter, prob, result) {
  bestTH <- 0
  bestAms <- 0
  res <- c()
  for (i in 0:iter) {
    threshold <- start + (0.1*i);
    
    pred <- rep('b', length(prob))
    pred [which(prob>threshold)]<- 's'
    
    result$Class <- pred
    
    ams <- getAMS(result)
    print(paste0(threshold,":",ams))
    
    if (ams > bestAms) {
      bestAms <- ams
      bestTH <- threshold
    }
    res <- append(res, ams)
  }
  plot(res, type="b")
  print(paste("Best Threshold: ", bestTH, ", AMS: ", bestAms))
}


getThresholdWeight <- function(start, iter, prob, result) {
  bestTH <- 0
  bestAms <- 0
  res <- c()
  for (i in 1:iter) {
    threshold <- start + (0.1*i);
    
    pred <- rep('b', length(prob))
    pred [which(prob<threshold)]<- 's'
    
    result$Class <- pred
    
    ams <- getAMS(result)
    if (ams > bestAms) {
      bestAms <- ams
      bestTH <- threshold
    }
    res <- rbind(res, c(threshold,ams))
  }
  plot(res, type="b")
  print(paste("Best Threshold: ", bestTH, ", Best AMS: ", bestAms))
}



getAMS <- function(result) {
  s <- sum(result$Weight[which(result$Class=='s' & result$Label=='s')])
  b <- sum(result$Weight[which(result$Class=='s' & result$Label=='b')])
  br <- 10
  ams <- sqrt(2*((s+b+br) * log(1+(s/(b+br))) -s))
  return (ams)
}

cutVars <- function(data) {
  numBins <- 300
  signal <- data$Signal
  
  data <- subset(data, select=-c(Signal))
  data <- cut(data, numBins, labels=FALSE)
  data$Signal <- signal
#   data$DER_mass_MMC <- cut(data$DER_mass_MMC, numBins, labels=FALSE)
#   data$DER_mass_transverse_met_lep <- cut(data$DER_mass_transverse_met_lep, numBins, labels=FALSE)
#   data$DER_mass_vis <- cut(data$DER_mass_vis, numBins, labels=FALSE)
#   data$DER_lep_eta_centrality <- cut(data$DER_lep_eta_centrality, numBins, labels=FALSE)
#   data$PRI_tau_pt <- cut(data$PRI_tau_pt, numBins, labels=FALSE)
#   data$DER_met_phi_centrality <- cut(data$DER_met_phi_centrality, numBins, labels=FALSE)
#   data$DER_mass_transverse_met_lep <- cut(data$DER_mass_transverse_met_lep, numBins, labels=FALSE)
#   data$DER_mass_vis <- cut(data$DER_mass_vis, numBins, labels=FALSE)
#   data$DER_lep_eta_centrality <- cut(data$DER_lep_eta_centrality, numBins, labels=FALSE)
  return(data);
}

