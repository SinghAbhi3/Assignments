#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Neural Network')

#Loading required packages
library(plyr)
library(neuralnet)
library(nnet)
#install.packages('NeuralNetTools')
library(NeuralNetTools)
library(caTools)

concrete <- read.csv('concrete.csv')
head(concrete)
concrete <- as.data.frame(concrete)

#Plot
hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))


hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))

#Normalizing the data
maxs <- apply(concrete,2,max)
maxs
mins <- apply(concrete,2,min)
mins

scaled.data <- scale(concrete, center = mins, scale = maxs-mins)
scaled <- as.data.frame(scaled.data)

split <- sample.split(scaled$strength, SplitRatio = 0.8)
train <- subset(scaled, split==T)
test <- subset(scaled, split==F)

n <- names(train)
n
f <- as.formula(paste('strength~', paste(n[!n %in% 'strength'], collapse = '+')))

#First model with hidden = 2
nn_1 <- neuralnet(f, data = train, hidden = 2,linear.output = TRUE )

#Prediction

predicted_nn_1 <- compute(nn_1, test[1:8])
str(predicted_nn_1)

true.prediction <- predicted_nn_1$net.result * (max(concrete$strength)-min(concrete$strength))+min(concrete$strength)

#Convert the test data
test.r <- (test$strength)*(max(concrete$strength)-min(concrete$strength))+min(concrete$strength)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_1 , rep = "best")

#Second model with hidden = 3
nn_2 <- neuralnet(f, data = train, hidden = 3,linear.output = TRUE )

#Prediction

predicted_nn_2 <- compute(nn_2, test[1:8])
str(predicted_nn_1)

true.prediction <- predicted_nn_2$net.result * (max(concrete$strength)-min(concrete$strength))+min(concrete$strength)

#Convert the test data
test.r <- (test$strength)*(max(concrete$strength)-min(concrete$strength))+min(concrete$strength)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_2 , rep = "best")

#Third model with hidden = 4
nn_3 <- neuralnet(f, data = train, hidden = 4,linear.output = TRUE )

#Prediction

predicted_nn_3 <- compute(nn_3, test[1:8])
str(predicted_nn_3)

true.prediction <- predicted_nn_3$net.result * (max(concrete$strength)-min(concrete$strength))+min(concrete$strength)

#Convert the test data
test.r <- (test$strength)*(max(concrete$strength)-min(concrete$strength))+min(concrete$strength)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_3 , rep = "best")

#Third model with hidden = 5

nn_4 <- neuralnet(f, data = train, hidden = 5,linear.output = TRUE )

#Prediction

predicted_nn_4 <- compute(nn_4, test[1:8])
str(predicted_nn_4)

true.prediction <- predicted_nn_4$net.result * (max(concrete$strength)-min(concrete$strength))+min(concrete$strength)

#Convert the test data
test.r <- (test$strength)*(max(concrete$strength)-min(concrete$strength))+min(concrete$strength)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_4 , rep = "best")


#The best model is nn_4