rm(list=ls(all=TRUE)) 
setwd("/home/dev/work/Insofe/20140119-KNN/Collab-Filtering/Music/kaggle_visible_evaluation_triplets")

# Read User data and assign ID
users.data=read.table(file="users.txt",header=FALSE)
user.id <- c(1:nrow(users.data))
users.data <- cbind(users.data,  user.id)
colnames(users.data) <- c("User", "UserID")
head(users.data)
rm(user.id)

# Read Song data and assign ID
songs.data=read.table(file="songs.txt",header=FALSE,sep=" ")
colnames(songs.data) <- c("Song", "SongID")
head(songs.data)

# Read the music file
rating.data=read.table(file="kaggle_visible_evaluation_triplets.txt", sep="\t", header=FALSE)
#rating.data <- rating.data[1:1000,]
colnames(rating.data)=c("User","Song","Downloads")
rating.data <- merge(x=rating.data, y=users.data, by.x="User", by.y="User")
rating.data <- merge(x=rating.data, y=songs.data, by.x="Song", by.y="Song")
rating.data <- subset(rating.data, select=-c(Song,User))
rm(users.data)
head(rating.data)

#Create a sparse matrix of ratings
library(Matrix)
sm <- sparseMatrix(rating.data$UserID,rating.data$SongID, x=rating.data$Downloads)
rownames(sm) <- c(paste("u",1:nrow(sm),sep=""))
colnames(sm) <- c(paste("s",1:ncol(sm),sep=""))

#Use the following in case we run into memory issues
sm <- sm[,which(colSums(sm)>200)]
#sm <- (sm[which(rowSums(sm)>200),)

library(recommenderlab)
min.songs.rated <- 20

#Create a real Rating matrix from sparseMatrix
rrm <- new ("realRatingMatrix", data=sm)
#Pick up rows which have 10 ratings or more
rrm <- rrm[which(rowCounts(rrm) > min.songs.rated),]
rrm

#Create recommender
recom <- Recommender(rrm[1:1000], method = "POPULAR")
#Predict song recommendations
pred <- predict(recom, rrm[1001:1005], n=5)
as(pred,"list")

rm(recom,pred)


pred.mae <- c()
for (i in 1:10) {
  #Evaluate recommender
  ev <- evaluationScheme(rrm[1:1000], method="split", 
                         train=0.9, given=10, goodRating=1)
  
  #UBCF recommender error calculation
  r1 <- Recommender(getData(ev, "train"), "UBCF")
  p1 <- predict(r1, getData(ev, "known"), type="ratings")
  pred.err <- calcPredictionError(p1, getData(ev, "unknown"))
  pred.mae <- rbind(pred.mae,pred.err[2])
  i < i+1
}
pred.mae
mean(pred.mae)


