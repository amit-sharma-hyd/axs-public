source("MusicRecommenderLab.R")
sm <- sm[which(rownames(sm) %in% rownames(rrm)),]

#Cosine distance function - it actually returns the cosine similarity
#but since we are looking for nearest neighbors, we can still use it
#with the right order
fn_cosine_dist.ratings=function(q,D) {
  dist=c(0,0)
  dist=data.frame(t(dist))
  names(dist) <- c("UserID", "Distance")
  for (i in 1:nrow(D)) {
    x=q
    y=D[i,]
    xy=rbind(q,D[i,])
    d=data.frame(cbind(rownames(D)[i],(x %*% y) / (sqrt(x%*%t(x) * y%*%y))))
    names(d) <- c("UserID", "Distance")
    dist=rbind(dist,d)
  }
  dist[2:nrow(dist),]
}

manualRecomMSE <- function() {
  # Evaluate the MSE for our algorithm
  sum.err.sq <- 0
  
  #We will store actual and pred outcomes in these
  actuals <-c()
  preds <- c()
  
  ratings.actual <-c()
  ratings.pred <- c()
  ratings.user.mean <-c()
  ratings.user.sd <- c()
  ratings.others.mean <- c()
  
  num.recos <- 0
  
  #Lets split training and test data
  all.rows <- 1:1000
  train.rows <- sample(all.rows,900,replace=FALSE)
  sm.train <- sm[train.rows,]
  sm.test <- sm[all.rows[-train.rows], ]
  
  num.given <- 10
  
  for (i in 1:nrow(sm.test)) {
    #Collect user's ratings
    sel.user.ratings <- sm.test[i,]
    #Which songs has the user downloaded
    sel.user.songs <- names(sm.test[i,which(sm.test[i,] != 0)])
    sel.user.songs
    
    #Find out other users who rated songs (one or more) which selected user has rated
    sel.songs.ratings <- sm.train[,sel.user.songs]
    
    #Randomly select some songs to be evaluated and remove it from the list for NN calc
    sel.random.songs <- sample(sel.user.songs,length(sel.user.songs)-10)
    
    #Find out other users who rated songs (one or more) which selected user has rated
    sel.songs.ratings <- sm.train[,setdiff(sel.user.songs,sel.random.songs)]
    
    #Pick up those users and ratings which have at least 5 'other' songs rated
    sel.songs.ratings <- sel.songs.ratings[apply(sel.songs.ratings,1,function(x) sum(which(x>0))>5),]
    
    #Let us try to predict only if there are at least 5 other users who have heard the 
    #same songs as X (other than the randomly selected songs for eval)
    if (nrow(sel.songs.ratings)>5) {
      #Normalize the ratings
      library(vegan)
      sel.songs.ratings <- decostand(sel.songs.ratings,method="range",MARGIN=1) 
      
      
      #The selected user ratings for songs he has rated is our query
      query <- sm.test[i,setdiff(sel.user.songs,sel.random.songs)]
      
      #Find distance of the selected user from other users
      dist.ratings <- fn_cosine_dist.ratings(t(query),sel.songs.ratings)
      
      #Find Top n nearest neighbors
      nn.max <- nrow(dist.ratings)
      if (nrow(dist.ratings) > 20) {
        nn.max <- 20
      }
      NN<-dist.ratings[order(dist.ratings$Distance,decreasing=FALSE),][1:nn.max,]
      
      #Now pick normalized NN ratings for the randomly selected song
      NN.ratings.random.songs <- sm.train[row.names(sm.train) %in% NN$UserID, ]
      NN.ratings.random.songs <- decostand(NN.ratings.random.songs, method="range", MARGIN=1)
      NN.ratings.random.songs <- NN.ratings.random.songs[,sel.random.songs]
      
      # Get stats for selected user ratings such as mean, min, max, range, etc
      sel.user.mean <- mean(sel.user.ratings[which(sel.user.ratings != 0)])
      sel.user.sd <- sd(sel.user.ratings[which(sel.user.ratings != 0)])
      sel.user.min <- min(sel.user.ratings[which(sel.user.ratings != 0)])
      sel.user.max <- max(sel.user.ratings)
      range <- sel.user.max-sel.user.min
      
      for (sel.random.song in sel.random.songs) {
        actual.rating <- sm.test[i,sel.random.song]
        NN.ratings.random.song <- NN.ratings.random.songs[,sel.random.song]
        #       NN.ratings.random.song <- NN.ratings.random.songs[which(NN.ratings.random.songs > 0)]
        avg.users.rating <- mean(NN.ratings.random.song)
        pred.rating <- sel.user.min+(range*avg.users.rating)
        sum.err.sq <-  sum.err.sq + (pred.rating-actual.rating)^2
        
        ratings.actual <- cbind(ratings.actual,actual.rating)
        ratings.pred <- cbind(ratings.pred,pred.rating)
        
        ratings.user.mean <-cbind(ratings.user.mean,sel.user.mean)
        ratings.user.sd <- cbind(ratings.user.sd,sel.user.sd)
        ratings.others.mean <- cbind(ratings.others.mean,avg.users.rating)
        
        # Store Like-Dislike information
        if (actual.rating > sel.user.mean) {
          actuals <- rbind(actuals,"Like")
        } else {
          actuals <- rbind(actuals,"Dislike")
        }
        
        if (pred.rating > sel.user.mean) {
          preds <- rbind(preds,"Like")
        } else {
          preds <- rbind(preds,"Dislike")
        }
        num.recos <- num.recos+1    
      }
    }
  }
  
  # Calculate MSE
  mse <- sum.err.sq / num.recos
  return(mse)  
}

pred.mse <- c()
for (i in 1:10) {
  pred.mse <- rbind(pred.mse,manualRecomMSE())
  i < i+1
}
pred.mse
mean(pred.mse)

