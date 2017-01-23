geneticAlgo<- function(population,fitnessFun,mutate, 
                       crossOver, mutProb,elite, maxiterations){
  
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize=nrow(population)
  topElite=round(elite*origPopSize,0)
  
  fitN=apply(population,1,fitnessFun)
  
  population=data.frame(population,fitN)
  
  population=population[sort.list(population[,ncol(population)]), ]

  # Main loop
  for (i in 1:maxiterations) {
    population=population[1:topElite,]
    population=population[,-c(ncol(population))]
    rownames(population)=seq(1:nrow(population))
    mut =mutProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(population)<origPopSize) {
      
      # Mutation
      if (runif(1,0,1)<mut) {
        c=sample(1:topElite,1)
        population[nrow(population)+1,]=mutate(population[c,])
      }
      
      # Crossover
      else {
        c1=sample(1:topElite,1)
        c2=sample(1:topElite,1)
        bred <- crossOver(population[c1,],population[c2,])
        population[nrow(population)+1,]=bred
      }
    }
    
    fitN=apply(population,1,fitnessFun)
    population=data.frame(population,fitN)
    population=population[sort.list(population[,ncol(population)]), ]
    
    # Print current best score
    
    cat("best fitness score in iteration", i, "=", 
        population[1,ncol(population)], "\n")
    
    if (population[1,ncol(population)]==0){
      return(population[1,])
      i=maxiterations
    }
    #returns the best solution   
    
  }
  return(population[1,])
}

