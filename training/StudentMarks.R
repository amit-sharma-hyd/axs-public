rm(list=ls())
par(mfrow=c(1,1))

# Set the working directory
setwd('~/axs/docs/branding/Rajamundhry/')

# Load the dataframe from the CSV file
stMarksDF = read.csv('Students_Marks_2.csv')

# Check the data frame
head(stMarksDF)

# Check the datatypes of marks DF
str(stMarksDF)

# Remove NA
stMarksDF = stMarksDF[complete.cases(stMarksDF),]


# Find total Marks for each student
colfunc <- colorRampPalette(c("green", "orange"))
par(las=2) 
stTotalMarks = apply(marksDF,1,sum)
stRank = stTotalMarks[order(stTotalMarks, decreasing = T)]
barplot(stRank[1:10], names=stMarksDF[names(stRank[1:10]),]$Roll.No., col=colfunc(10))

# Find the topper

# Take out Roll Nos for the time being
marksDF = subset(stMarksDF, select=-c(Roll.No.))

# Tabulate pass vs fail
passFail = table(marksDF>=40)
pie(passFail, col = c('red', 'blue'), main="Passed Vs Failed", cex=0.8,
    labels=paste0(names(passFail), " - ", passFail))

table(any(marksDF))

# Summarize the data in the marks DF
summary(marksDF)

# Set plot window to single chart
par(mfrow=c(1,1))

# Checking range and variance of marks across subjects
boxplot(marksDF, outline=F, col=rainbow(12:14))

# Check the mean of each student
barplot(sapply(marksDF, mean), col=rainbow(11:14))

# Check correlation of marks
cr = round(cor(marksDF), 1)

# Using library corrplot
library(corrplot)
corrplot(cr)
# Check correlation plot for mtcars
corrplot(cor(mtcars), type="lower", method="circle")

# Histogram plots
hist(stMarksDF$MEFA, col=rainbow(12:14))

par(mfrow=c(2,4))
for (i in seq(1,8)) {
  barplot(as.matrix(marksDF)[i,], col=rainbow(12:13))
}

# Combining plots
par(mfrow=c(2,1))
boxplot(marksDF, outline=F, col=rainbow(12:14))
barplot(sapply(marksDF, mean), col=rainbow(11:14))
