#install.packages("DMwR")
# Load libraries
library(DMwR)
library(ggplot2)

# Import data
setwd("D:/études/DBS/Programming for big data/CA5")
churn_data <- read.csv("churn.csv")

# Attach data
attach(churn_data)

# Understanding the data
str(churn_data)
head(churn_data)
summary(churn_data)

# Convert SeniorCitizen to factor
#churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)
churn_data$SeniorCitizen <- as.factor(ifelse(churn_data$SeniorCitizen == 1, "Yes", "No"))

# Check missing values
sum(is.na(churn_data))

# Remvove unecessary column "Customer ID" and NA records (only 11 rows)
churn_data <- churn_data[-1]
churn_data <- na.omit(churn_data)

# Boxplot "Monthly charges distribution by churn"
boxplot(MonthlyCharges ~ Churn, ylab="Monthly charges", main="Monthly charges distribution by churn")

# Proportion of churning customers (senior vs non-senior citizens)
ggplot(churn_data, aes(SeniorCitizen, fill=Churn))+
  geom_bar(position= "fill")+
  ylab("Proportion")+
  xlab("Senior citizen")+
  ggtitle("Proportion of churning customers (senior vs non-senior citizens)")


# Split data into training and test sets
set.seed(123)
ind <- sample(2, nrow(churn_data), replace=TRUE, prob=c(0.70, 0.30))
train <- churn_data[ind==1,]
test <- churn_data[ind==2,]
summary(train)

# Oversampling data using SMOTE technic
train <- SMOTE(Churn ~ ., train, perc.over=200, pc.under=0)

# Find important variables to build logistic regression model
logit_model <- glm(Churn ~ ., family=binomial, data=train)
summary(logit_model)
confint(logit_model)

# Build final model using significant variables
logit_final <- glm(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines
                    + InternetService + TechSupport + Contract + PaymentMethod + MonthlyCharges, 
                    family=binomial, data=train)

# Predictions on test dataset
predictions <- predict(logit_final, test, type="response")

y_pred <- ifelse(predictions > 0.5, "Yes", "No")
y_act <- test$Churn

# Model accuracy.This metric cannot be trusted since we are dealing with an imballanced dataset
model_accuracy <- mean(y_pred == y_act)

confusion_matrix <- table(predicted=y_pred, actual=y_act)
confusion_matrix

# Pecentage of correct predictions that customer is churning
confusion_matrix[2,2]/(confusion_matrix[2,1] + confusion_matrix[2,2])

detach(churn_data)
