# Git hub repository URL: https://github.com/gregorycannelladbs/R-Data-Interpretation

# Load library
library(ggplot2)

# Import data
setwd("D:/études/DBS/Programming for big data/CA5")
lungcapdata <- read.delim("LungCapData.txt")

attach(lungcapdata)

# Understanding the data
str(lungcapdata)
head(lungcapdata)
summary(lungcapdata)

table(Smoke, Age)
table(Gender, Age)

# Caclulate Lungcap mean by Gender
aggregate(LungCap ~ Gender, data=lungcapdata, mean)

# Calculate Lungcap mean by Gender using filtering just to demonstrate that I know how to filter
mean(lungcapdata$LungCap[lungcapdata$Gender == "male"]) 
mean(lungcapdata$LungCap[lungcapdata$Gender == "female"])

# Pearson correlation coefficient
cor(Age, LungCap)

# Gender frequency table
gender_counts <- table(Gender)
barplot(gender_counts, main="Gender frequency table",
        xlab="Gender", ylab="count")

# Boxplot LungCap by Gender
boxplot(LungCap ~ Gender, main="Lungcap distribution by Gender")

# Scatter plot LungCap by Age
plot(Age, LungCap, main="Lungcap distribution by Age using scatterplot")

# Line plot of average Lungcap by Age (male vs female)
ggplot(aggregate(LungCap ~ Gender + Age, data=lungcapdata, mean), aes(Age, LungCap, color= Gender))+
  geom_line()+
  ggtitle("Average Lungcap by Age (male vs female)")

# Simple linear regression model only using one variable ("Age").
# Our model using only the variable "Age" gives us an adjusted R-squared of 0.67
# This represents the percentage of variation of the dependent variable ("LungCap") that is explained by our
# independent variable ("Age")
lm_model <- lm(LungCap ~ Age, data=lungcapdata)
summary(lm_model)

# Adding linear regression line to our scatter plot
plot(Age, LungCap, main="Linear regression line")
abline(lm_model, col='red', lwd=2)

# Diagnostic plots to check linear regression assumptions
# The "Residuals VS Fitted" plot shows that linearity assumption is met (i.e: the red line is flat).
# It also shows that the variation is fairly constant (i.e: the residuals have cloud/rectangular shape).
# The "Normal Q-Q" plot shows that the residuals are normally distributed (i.e: the residuals fall on the diagonale).
par(mfrow=c(2,2))
plot(lm_model)

# Multiple linear regression model using all variables but "Caesarean" since it does not make sense conceptually.
# By using several significant independent variables, the model gives us a much better adjusted R-squared of 0.85
mlm_model <- lm(LungCap ~ Age + Height + Smoke + Gender, data=lungcapdata)
summary(mlm_model)

plot(mlm_model)
par(mfrow=c(1,1))

detach(lungcapdata)
