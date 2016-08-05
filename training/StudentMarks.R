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


# Take out Roll Nos for the time being
marksDF = subset(stMarksDF, select=-c(Roll.No.))
# Summarize the data in the marks DF
summary(marksDF)

# Tabulate pass vs fail
passFail = table(marksDF>=40)
pie(passFail, col = c('red', 'blue'), main="Passed Vs Failed", cex=0.8,
    labels=paste0(names(passFail), " - ", passFail))

# Find total Marks for each student
stTotalMarks = apply(marksDF,1,sum)

# Find the topper
colfunc <- colorRampPalette(c("green", "yellow", "red"))
par(las=2) 
N = 20
stRank = stTotalMarks[order(stTotalMarks, decreasing = T)]
barplot(stRank[1:N], names=stMarksDF[names(stRank[1:N]),]$Roll.No., col=colfunc(N))


# Set plot window to single chart
par(mfrow=c(1,1))

# Checking range and variance of marks across subjects
boxplot(marksDF, outline=F, col=rainbow(12:14))

# Check the mean of each subject
barplot(sapply(marksDF, mean), col=rainbow(11:14))

# Check correlation of marks
cr = round(cor(marksDF), 1)

# Using library corrplot
library(corrplot)
corrplot(cr)
# Check correlation plot for mtcars
# corrplot(cor(mtcars), type="lower", method="circle")

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

#########################
# REGRESSSION ANALYSIS  #
#########################

# Lets take some students as test set and remaining as test
N = nrow(marksDF)
trainRows = sample(1:N, N*0.9, replace=F)
train = marksDF[trainRows,]
test = marksDF[-trainRows,]

# Now lets fit the model
head(train)
# Let's make PS score as output variable and all others input
model = lm(MFCS~.,train)
summary(model)
preds = predict(model, newdata = test)
res = data.frame(test$PS, round(preds,0))
res
names(res) = c("Actual", "Pred")
mse = sum((res$Actual-res$Pred)^2)/nrow(res)
mse
# Is the MSE any better than mean of train output
sum((res$Actual-mean(train$MFCS))^2)/nrow(res)


#########################
# LOGISTIC REGRESSSION  #
#########################

# Lets find a good cutoff for marks to be classified as Great Score (1) or not (0)
summary(marksDF$MFCS)
table(marksDF$MFCS>70)
marksDF$MFCS_Great_Score = ifelse(marksDF$MFCS>70, 1, 0)

# Now we shall remove the MFCS col to remove direct calculation
df = marksDF
df$MFCS = NULL

# Lets take some students as test set and remaining as test
N = nrow(df)
trainRows = sample(1:N, N*0.9, replace=F)
train = df[trainRows,]
test = df[-trainRows,]

model <- glm(MFCS_Great_Score ~.,family=binomial(link='logit'),data=train)

summary(model)
preds = predict(model, newdata = test, type = "response")
res = data.frame(test$MFCS_Great_Score, round(preds,0))
names(res) = c("Actual", "Pred")
res
