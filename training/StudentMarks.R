library(party)
rm(list=ls())

# Set the working directory
setwd('.')

# Load the dataframe from the CSV file
stMarksDF = read.csv('Students_Marks_2.csv')

stMarksDF$Roll.No. = NULL

# Check the data frame
head(stMarksDF)

# Check the datatypes of marks DF
str(stMarksDF)

# Remove NA
stMarksDF = stMarksDF[complete.cases(stMarksDF),]


# Take out Roll Nos for the time being
marksDF = stMarksDF #subset(stMarksDF, select=-c(Roll.No.))
# Summarize the data in the marks DF
summary(marksDF)

# Tabulate pass vs fail
passFail = table(marksDF>=40)
pie(passFail, col = c('red', 'green'), main="Passed Vs Failed", cex=0.8,
    labels=paste0(names(passFail), " - ", passFail))

# Find total Marks for each student
stTotalMarks = apply(marksDF,1,sum)

# Max total marks
max(stTotalMarks)

which(stTotalMarks == max(stTotalMarks))

# Find the topper
# Show only one plot in the entire plot area
par(mfrow=c(1,1))
colfunc <- colorRampPalette(c("green", "yellow", "red"))
N = 20
stRank = sort(stTotalMarks, decreasing = T)
barplot(stRank[1:N], main="Top 20 Students", 
        names=stMarksDF[names(stRank[1:N]),]$Roll.No., col=colfunc(N))


# Set plot window to single chart
par(mfrow=c(1,1), cex=0.7, las=1)
# Checking range and variance of marks across subjects
boxplot(marksDF, outline=F, main="Marks distribution in each subject", col=rainbow(12:14))

# Check the mean of each subject
barplot(sapply(marksDF, mean), main="Avg Marks in each Subject", col=rainbow(11:14))

# Check correlation of marks
cr = round(cor(marksDF), 1)

# Using library corrplot
library(corrplot)
corrplot(cr, type="lower")
# Check correlation plot for mtcars
# corrplot(cor(mtcars), type="lower", method="circle")

# Histogram plots
hist(stMarksDF$MEFA, col=rainbow(12:14))

# Combining plots
par(mfrow=c(1,2), cex=0.8)
boxplot(marksDF, outline=F, col=rainbow(12:14))
barplot(sapply(marksDF, mean), col=rainbow(11:14))

#########################
# REGRESSSION ANALYSIS  #
#########################

# Lets take some students as test set and remaining as test
N = nrow(marksDF)
# Randomly sample 90% of records to be used for train
trainRows = sample(1:N, N*0.9, replace=F)
# Use the sample to separate train/test from df
train = marksDF[trainRows,]
test = marksDF[-trainRows,]

# Now lets fit the model
head(train)
# Let's make PS score as output variable and all others input
model = lm(MFCS~.,train)
summary(model)
preds = predict(model, newdata = test)
res = data.frame(test$MFCS, round(preds,0))
names(res) = c("Actual", "Pred")
res
mse = sum((res$Actual-res$Pred)^2)/nrow(res)
mse
# Is the MSE any better than mean of train output
sum((res$Actual-mean(train$MFCS))^2)/nrow(res)


#############################
# CLASSIFICATION TECHNIQUES #
#############################

# Lets create some data for classification algorithm
df = marksDF

# Lets find a good cutoff for marks to be classified as Great Score (1) or not (0)
cutoffMarks = 70
summary(df$MFCS)
table(df$MFCS>cutoffMarks)
# Set the new column to be 1 if MFCS is greater than 70 else 0
df$MFCS_Great_Score = ifelse(marksDF$MFCS>cutoffMarks, 1, 0)
df$MFCS_Great_Score = as.factor(df$MFCS_Great_Score)
# Now we shall remove the MFCS col to remove direct calculation
df$MFCS = NULL

# Lets take some students as test set and remaining as test
# Using library caTools to create a uniform train/test 
library(caTools)
trainRows = sample.split(df$MFCS_Great_Score, SplitRatio=0.9)
train = df[trainRows,]
test  = df[!trainRows,]

############################
# 1- LOGISTIC REGRESSSION  #
############################

model <- glm(MFCS_Great_Score ~.,family=binomial(link='logit'),data=train)

summary(model)
preds = predict(model, newdata = test, type = "response")
res = data.frame(test$MFCS_Great_Score, round(preds,0))
names(res) = c("Actual", "Pred")
res


#################################
#        DECISION TREE          #
#################################
# We can use the same train/test as above (from Logistic regression)
# Conditional Inference Tree 
fit <- ctree(MFCS_Great_Score ~.,data=train)
# Lets print the tree to see what it has learnt
plot(fit, main="Conditional Inference Tree for Great Score")
preds = predict(fit, newdata = test, type = "response")
res = data.frame(test$MFCS_Great_Score, preds)
names(res) = c("Actual", "Pred")
res

