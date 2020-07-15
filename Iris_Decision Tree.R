#Reading-in the data set

View(iris)
head(iris)
str(iris)

#Training and Testing Sample Data
set.seed(1234) #To get reproducible result
splitteddata <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[splitteddata==1,]
testData <- iris[splitteddata==2,]

#install.packages('party')
library(party)

myirisdata <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
myirisdata_ctree <- ctree(myirisdata, data=trainData)
#Conditional inference tree with 4 terminal nodes

#Response:  Species 
#Inputs:  Sepal.Length, Sepal.Width, Petal.Length, Petal.Width 
#Number of observations:  110 

#1) Petal.Length <= 1.9; criterion = 1, statistic = 103.725
#2)*  weights = 37 
#1) Petal.Length > 1.9
#3) Petal.Width <= 1.6; criterion = 1, statistic = 47.974
#4) Petal.Length <= 4.6; criterion = 1, statistic = 16.904
#5)*  weights = 30 
#4) Petal.Length > 4.6
#6)*  weights = 8 
#3) Petal.Width > 1.6
#7)*  weights = 35

myirisdata_ctree

#Checking the prediction
table(predict(myirisdata_ctree), trainData$Species)

print(myirisdata_ctree)

plot(myirisdata_ctree)
plot(myirisdata_ctree, type="simple")

#Test data

testPred <- predict(myirisdata_ctree, newdata = testData)
table(testPred, testData$Species)

#testPred     setosa versicolor virginica
#setosa         13          0         0
#versicolor      0         14         0
#virginica       0          1        12