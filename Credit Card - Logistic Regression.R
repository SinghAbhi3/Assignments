getwd()
setwd('C:\\Users\\singh\\Documents\\LR ExcelR')
creditcard <- read.csv('creditcard (1).csv')
head(creditcard)
library(dplyr)

# Removing column X
creditcard <- select(creditcard,-X)
head(creditcard)
str(creditcard)
table(creditcard$reports)
table(creditcard$selfemp)
creditcard$majorcards <- sapply(creditcard$majorcards,factor)
str(creditcard)
creditcard$dependents <- sapply(creditcard$dependents,factor)
creditcard$selfemp <- sapply(creditcard$selfemp,factor)


#Checking for missing values
library(Amelia)
missmap(creditcard,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
#There are no missing values


#Data Visualization
library(ggplot2)
ggplot(creditcard,aes(age)) + geom_histogram(aes(fill=factor(selfemp)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(age)) + geom_histogram(aes(fill=factor(owner)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(age)) + geom_histogram(aes(fill=factor(majorcards)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(age)) + geom_histogram(aes(fill=factor(reports)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(age)) + geom_histogram(aes(fill=factor(card)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(income)) + geom_histogram(aes(fill=factor(dependents)),color='black',binwidth=5) + theme_bw()

ggplot(creditcard,aes(income)) + geom_histogram(aes(fill=factor(selfemp)),color='black',binwidth=5) + theme_bw()


# Buliding Model

library(caTools)

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(creditcard$card, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(creditcard, sample == TRUE)

# Testing Data
test = subset(creditcard, sample == FALSE)

model = glm(card ~ ., family = binomial(logit), data = train)
summary(model)

new.step.model <- step(model)
summary(new.step.model)

test$predicted.card = predict(new.step.model, newdata=test, type="response")
table(test$card, test$predicted.card > 0.5)
#Confusion Matrix

#       FALSE TRUE
#  no     89    0
#  yes     6  301

#Accuracy
acc <- (89+301)/(89+301+0+6)
acc

# The accuracy of the given model is high


#Precision

pre <- 89/(89+6)
pre

#Recall
rec <- 89/(89+0)
rec

# With the given model it will be very easy to decide whether the application for a credit card should be accepted or not. 


