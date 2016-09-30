#########################################################################
# Learning Statistical Data Analysis using                              #
# Kaggle data for House Prices                                          #
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques  #
#########################################################################

rm(list=ls())

# Set the working directory
setwd('~/axs/kaggle/house-prices/')

# Let's set scientific notation 
options(scipen=10)

# Load the dataframe from the CSV file
data = read.csv('data/train.csv')

# Initialize a few color pallettes
myCols = rainbow(8:13)

#################################################
#           EXPLORATORY ANALYSIS                #
#################################################

# Column types
str(data)

# Column Summaries
summary(data)

# Let's check the price distribution using a histogram
hist(data$SalePrice)
# Let's color it up a bit
hist(data$SalePrice, col=myCols)

# Another way to explore the price distribution
boxplot(data$SalePrice)

# Get rid of outliers on the boxplot
boxplot(data$SalePrice, outline=F, main="Sale Price Distribution")

# If we are dealing with just one data frame, it helps to attach it so 
# we can just use the column names
attach(data)

# Check the data frame
head(data)

# Check the datatypes of marks DF
str(data)

# Lets see how many Bldg Types are there
barplot(table(BldgType), col=myCols)

par(mfrow=c(1,2), cex=0.8, las=2)
# Lets check the prices boxplot across Bldg Types
boxplot(SalePrice ~ BldgType, col=myCols, outline=F,
        main="Sale Price across Bldg Types")

# Lets craete a new column for House Rate
SaleRate = SalePrice / LotArea

# Lets check the prices boxplot across Bldg Types
boxplot(SaleRate ~ BldgType, col=myCols, 
        outline=F, main="SaleRate across Bldg Types")

par(mfrow=c(1,1), cex=0.8, las=2)
boxplot(SalePrice ~ OverallCond+BldgType, col=myCols, outline=F,
        main="Sale Price across Overall Condition")

# Calculate Years Old
data$YearsOld = 2016 - YearBuilt
attach(data)
plot (YearsOld, SalePrice)

# Lets just look at Numeric columns for a while for 
# correlation analysis
numericCols = sapply(data,is.numeric)
data = data[,numericCols]

# Check correlation of marks
# cr = round(cor(data), 1)
# 
# # Using library corrplot
# library(corrplot)
# corrplot(cr, type="lower")

# Lets remove the NAs
data = data[complete.cases(data),]

# Combining plots
#par(mfrow=c(1,2), cex=0.8, las=2)
#boxplot(marksDF, outline=F, col=rainbow(12:14))
#barplot(sapply(marksDF, mean), col=rainbow(11:14))

#########################
# REGRESSSION ANALYSIS  #
#########################

# Lets take some students as test set and remaining as test
N = nrow(data)
# Randomly sample 90% of records to be used for train
trainRows = sample(1:N, N*0.9, replace=F)
# Use the sample to separate train/test from df
train = data[trainRows,]
test = data[-trainRows,]

# Now lets fit the model
head(train)
# Let's make PS score as output variable and all others input
model = lm(SalePrice~MSSubClass+LotArea+OverallQual+
             OverallCond+MasVnrArea+YearsOld,
           data=train)
summary(model)
preds = predict(model, newdata = test)

# Lets look at top 10 predictions and actual values
# just to visually compare
data.frame(test$SalePrice[1:10],preds[1:10])

res = data.frame(test$SalePrice, round(preds,0))
names(res) = c("Actual", "Pred")
res
mse = sum((res$Actual-res$Pred)^2)/nrow(res)
mse
# Is the MSE any better than mean of train output
naiveMSE = sum((res$Actual-mean(train$SalePrice))^2)/nrow(res)
naiveMSE

#############################
# CLASSIFICATION TECHNIQUES #
#############################

# Lets create some data for classification algorithm
df = data

boxplot(SalePrice)
# Lets find a good cutoff /threshold for 
# price
highPriceThreshold = 250000
summary(df$SalePrice)
table(df$SalePrice>highPriceThreshold)
# Set the new column to be 1 if SalePrice is greater than threshold else 0
df$Costly = ifelse(df$SalePrice>highPriceThreshold, 1, 0)
df$Costly = as.factor(df$Costly)

# Now we shall remove the Sale Price col to remove direct calculation
test$SalePrice = NULL

# Lets take some students as test set and remaining as test
# Using library caTools to create a uniform train/test 
library(caTools)
trainRows = sample.split(df$Costly, SplitRatio=0.9)
train = df[trainRows,]
test  = df[!trainRows,]

############################
# 1- LOGISTIC REGRESSSION  #
############################

model <- glm(Costly ~.,family=binomial(link='logit'),data=train)

summary(model)
preds = predict(model, newdata = test, type = "response")
res = data.frame(df$Costly, round(preds,0))
names(res) = c("Actual", "Pred")
res
