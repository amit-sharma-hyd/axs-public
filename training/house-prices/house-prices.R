
#########################################################################
# Learning Statistical Data Analysis using                              #
# Kaggle data for House Prices                                          #
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques  # 
# author: Amit Sharma                                                   #
#########################################################################

rm(list=ls())

# Set the working directory
# setwd("C:\\work\\vamshi")
setwd('~/axs/github/axs-public/training/house-prices/')

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
boxplot(SalePrice ~ OverallCond, col=myCols, outline=F,
        main="Sale Price across Overall Condition")

# Calculate Years Old
data$YearsOld = 2016 - YearBuilt
attach(data)
plot (YearsOld, SalePrice, main="Sale Price Vs YearsOld")

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
model = lm(SalePrice~.,
           data=train)
summary(model)

# Pick up the most significant (***) parameters and rerun the model
model = lm(SalePrice~MSSubClass+LotArea+OverallQual+
             OverallCond+MasVnrArea+YearsOld,
           data=train)
summary(model)
preds = predict(model, newdata = test)



res = data.frame(test$SalePrice, round(preds,0))
names(res) = c("Actual", "Pred")
# Lets look at top 10 predictions and actual values
# just to visually compare
head(res,10)

# Lets calc Mean Squared Error
mse = sum((res$Actual-res$Pred)^2)/nrow(res)
mse
# Is the MSE any better than mean of train output (which would
# be our simplest/naiivest guess)
naiveMSE = sum((res$Actual-mean(train$SalePrice))^2)/nrow(res)
naiveMSE

# Naive Mean Squared error is bigger than our MSE (not bad)
naiveMSE/mse
