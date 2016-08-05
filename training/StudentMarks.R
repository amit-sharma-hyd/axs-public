rm(list=ls())

# Set the working directory
setwd('~/axs/docs/branding/Rajamundhry/')

# Load the dataframe from the CSV file
stMarksDF = read.csv('Students_Marks_2.csv')

# Check the data frame
head(stMarksDF)

# Check the datatypes of marks DF
str(stMarksDF)

# Remove NA
stMarksDF = complete.cases(stMarksDF)

# Find total Marks for each student




# Take out Roll Nos for the time being
marksDF = subset(stMarksDF, select=-c(Roll.No.))

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

barplot(c(marksDF[1,]), main="One student's marks")

# Combining plots
par(mfrow=c(2,1))
boxplot(marksDF, outline=F, col=rainbow(12:14))
barplot(sapply(marksDF, mean), col=rainbow(11:14))
