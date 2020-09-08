#Setting Working Directory

setwd('C:\\Users\\singh\\Documents\\KNN')

#Reading file

Glass <- read.csv('glass.csv')
View(Glass)
str(Glass)
summary(Glass)
Glass$Type <- as.factor(Glass$Type)

#Loading required packages

install.packages('e1071')
library(e1071)
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)

# Scaling or standardizing the data
Glass_standardized <- scale(Glass[,1:9])
head(Glass_standardized)
summary(Glass_standardized)#Data is standardized


#Joining the scaled data with the target column
combined_standard_glass <- cbind(Glass_standardized,Glass[10])
View(combined_standard_glass)

#Checking for missing values 
any(is.na(combined_standard_glass))
#FALSE

#We will now split the data(Test and Train)

#1-Setting seed
set.seed(100)

sample <- sample.split(combined_standard_glass$Type,SplitRatio = 0.70)

train <- subset(combined_standard_glass,sample==TRUE)

test <- subset(combined_standard_glass,sample==FALSE)


#K-Nearest Neighbour

Glass_KNN <- knn(train[1:9],test[1:9],train$Type,k=1)
Glass_KNN
#Finding Error in prediction
test$Type
Glass_KNN_Error <- mean(Glass_KNN!=test$Type)
Glass_KNN_Error
#0.2923077

#Building Confusion Matrix
confusionMatrix(Glass_KNN,test$Type)
#  Accuracy : 0.7385 or 73.85 %

#The above model is giving us 73.85% with k=1
#We can try for different values of k and for that we can use below mentioned code

Glass_KNN <- NULL
glass.error <- NULL
for (i in 1:10) {
  Glass_KNN <- knn(train[1:9],test[1:9],train$Type,k=i)
  glass.error[i] <- mean(Glass_KNN!=test$Type)
  
}

Glass_error <- as.data.frame(cbind(k=1:10,Glass_Error_Type = glass.error))

ggplot(Glass_error,aes(k,Glass_Error_Type))+ geom_point()+ geom_line() + 
scale_x_continuous(breaks=1:10)+ theme_bw() +xlab("Value of K") +ylab('Error')

#It can be seen from the graph that we are getting lowest error at k=3

#Building final model with k=3

Glass_KNN_Final <- knn(train[1:9],test[1:9],train$Type,k=3)
Glass_KNN_Final

#Finding Error in prediction

Glass_KNN_Error <- mean(Glass_KNN_Final!=test$Type)
Glass_KNN_Error
#0.2307692

#Building Confusion Matrix
confusionMatrix(Glass_KNN_Final,test$Type)
#Accuracy : 0.7692 or 76.92%

#The final value of k used is 3.

