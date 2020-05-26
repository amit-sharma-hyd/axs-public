rm(list=ls(all=TRUE)) 
setwd("/home/dev/work/Insofe/20140119-KNN/Collab-Filtering/Music/kaggle_visible_evaluation_triplets")

# Function to identify distances with each userid  based on song ratings
fn_dist.ratings=function(q,D) {
  dist=c(0,0)
  dist=data.frame(t(dist))
  names(dist) <- c("UserID", "Distance")
  for (i in 1:nrow(D)) {
    xy=rbind(q,D[i,])
    d=data.frame(cbind(rownames(D)[i],dist(xy, method="manhattan")))
    names(d) <- c("UserID", "Distance")
    dist=rbind(dist,d)
  }
  dist[2:nrow(dist),]
}

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
row.names(sm) <- c(1:nrow(sm))
colnames(sm) <- c(1:ncol(sm))

#Select a random user id
sel.user.id <- "147"
sel.user.ratings <- sm[sel.user.id,]
#Which songs has the user downloaded
sel.user.songs <- which(sel.user.ratings != 0)
sel.user.songs
#Find out other users who rated songs (one or more) which selected user has rated
sel.songs.ratings <- sm[,sel.user.songs]
sel.songs.ratings.sum <- rowSums(sel.songs.ratings)
sel.songs.ratings <- sel.songs.ratings[which(sel.songs.ratings.sum != 0),]

#Normalize the ratings
library(vegan)
sel.songs.ratings <- decostand(sel.songs.ratings,method="range",MARGIN=1) 



#The selected user ratings for songs he has rated is our query
query <- sm[row.names(sm)==sel.user.id,sel.user.songs]

#We need to calculate distance of the query from other users
sel.songs.ratings <- sel.songs.ratings[which(rownames(sel.songs.ratings) != sel.user.id),]
  
# identifying distances with each userid  based on song ratings
fn_dist.ratings=function(q,D) {
  dist=c(0,0)
  dist=data.frame(t(dist))
  names(dist) <- c("UserID", "Distance")
  for (i in 1:nrow(D)) {
    xy=rbind(q,D[i,])
    d=data.frame(cbind(rownames(D)[i],dist(xy, method="manhattan")))
    names(d) <- c("UserID", "Distance")
    dist=rbind(dist,d)
  }
  dist[2:nrow(dist),]
}
dist.ratings <- fn_dist.ratings(t(query),sel.songs.ratings)

#Find Top 10 nearest neighbors
NN10<-dist.ratings[order(dist.ratings$Distance,decreasing=FALSE),][1:10,]

#Get all the songs these top 10 NNs have listended to and rated
NN10.ratings <- sm[row.names(sm) %in% NN10$UserID,]

#Remove songs that selected user has already heard
NN10.ratings.other.songs <- NN10.ratings[,-c(sel.user.songs)]

#Sum up ratings for each song across NN10 users
NN10.song.rating.mean <- data.frame(colMeans(NN10.ratings.other.songs))
NN10.song.rating.mean <- cbind(colnames(NN10.ratings.other.songs),NN10.song.rating.mean)
colnames(NN10.song.rating.mean) <- c("SongID", "Rating")

#Pick the top 5 rated songs from NN10 users - they are the ones recommended for selected user
NN10.referred.songs <- NN10.song.rating.mean[order(NN10.song.rating.mean$Rating,decreasing=TRUE),][1:5,] 
NN10.referred.songs

songs.heard <- as.character(songs.data[which(songs.data$SongID %in% sel.user.songs),]$Song)
songs.recommended <- as.character(songs.data[which(songs.data$SongID %in% NN10.referred.songs$SongID),]$Song)

cat("\nUser", sel.user.id, "heard songs: ",songs.heard)
cat("\nSongs recommended:", songs.recommended)


# Evaluate results
# Lets take 100 random users and remove a rating from one of the songs 
# they have already heard and try to predict the rating for that song
# using our algorithm 


# 