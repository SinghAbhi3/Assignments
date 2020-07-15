data(iris)
iris_data <- iris
head(iris)

table(iris$Species)
#setosa versicolor  virginica 
#50         50         50 

plot1 <- boxplot(iris$Sepal.Length ~ iris$Species,ylab="Sepal.Length")
plot2 <- boxplot(iris$Sepal.Width ~ iris$Species,ylab="Sepal.Width")
plot3 <- boxplot(iris$Petal.Length ~ iris$Species,ylab="Petal.Length")
plot4 <- boxplot(iris$Petal.Width ~ iris$Species,ylab="Petal.Width")

plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species,pch=16)
legend( x="topleft", 
        legend=levels(as.factor(iris$Species)),
        col=c("black","red","green"), 
        pch=c(16) )

#RANDOM FOREST METHOD

#We will split the iris data into training and testing set

iris_split <- sample(2, nrow(iris),replace = TRUE, prob = c(0.7,0.3))
table(iris_split)
#iris_split
#1   2 
#109  41 

trainData <- iris_data[iris_split==1,]
dim(trainData)
testData <- iris_data[iris_split==2,]
dim(testData)

#Loading randomForest package

library(randomForest)

iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
print(iris_rf)

#Call:
 # randomForest(formula = Species ~ ., data = trainData, ntree = 100,      proximity = TRUE) 
#Type of random forest: classification
#Number of trees: 100
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 3.67%
#Confusion matrix:
 # setosa versicolor virginica class.error
#setosa         35          0         0  0.00000000
#versicolor      0         34         3  0.08108108
#virginica       0          1        36  0.02702703

table(predict(iris_rf),trainData$Species)

#Importance of the class descriptors
importance(iris_rf)

#We will build a random forest for our tsting data

irisPred<-predict(iris_rf,newdata=testData)
table(irisPred, testData$Species)


plot(margin(iris_rf,testData$Species))

#Checking the classification accuracy

print(sum(irisPred==testData$Species))
##The number of correct predictions is 38

print(length(testData$Species))
#datapoints used to test the prediction is 41

#Accuracy
print(sum(irisPred==testData$Species)/length(testData$Species))
# 0.9268293
