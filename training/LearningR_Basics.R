library(MASS)

##################################################
# BASIC DATA TYPES IN R                          #
##################################################
# List
x = c(1,2,3,4)
y = c('A', 'B', 'C', 'D')
paste0(x,y)

# Dataframe
df = data.frame(x,y)
df[df$x==2,]

##################################################
# BASIC DATA ANALYSIS IN R                       #
##################################################

# Descriptive Statistics
head(painters)
# Summarize data using basic stats
summary(painters)
# Tabulate data to get freq count
table(painters$School)
# Apply a single function to data frame
sapply(painters, mean)

##################################################
# PLOTTING IN R                                  #
##################################################

# Bar plot
freq = table(painters$School)
barplot(freq)
# Color it
barplot(freq, col = rainbow(12:14))
# Sort it
barplot(sort(freq), col = rainbow(12:14))
# Sort descending
barplot(sort(freq, decreasing = T), col = rainbow(12:14))

# Scatter plot to view eruption duration vs waiting time for volcanoes
head(faithful)
# Check on correlation
cor(faithful$eruptions, faithful$waiting)
# Plot the correlation
plot(faithful$eruptions, faithful$waiting, col = "red")
# Histogram to see distributions
hist(faithful$waiting)
# Boxplot the figures
boxplot(faithful$waiting)
# Boxplot group by
boxplot(Composition ~ School, data=painters, outline=F, col=rainbow(8))

