#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Neural Network')

#Loading required packages
library(plyr)
library(neuralnet)
library(nnet)
#install.packages('NeuralNetTools')
library(NeuralNetTools)
library(caTools)

df <- read.csv('50_Startups.csv')
head(df)


df$State <- as.numeric(revalue(df$State,
                               c("New York"="0", "California"="1",
                                 "Florida"="2")))

#Plots
plot(df$R.D.Spend, df$Profit)
plot(df$Administration, df$Profit)
plot(df$Marketing.Spend, df$Profit)
plot(df$State, df$Profit)

pairs(df)

cor(df)

df <- as.data.frame(df)
maxs <- apply(df,2,max)
maxs
mins <- apply(df,2,min)
mins

scaled.data <- scale(df, center = mins, scale = maxs-mins)
scaled <- as.data.frame(scaled.data)

split <- sample.split(scaled$Profit, SplitRatio = 0.7)
train <- subset(scaled, split==T)
test <- subset(scaled, split==F)

n <- names(train)
n
f <- as.formula(paste('Profit~', paste(n[!n %in% 'Profit'], collapse = '+')))

#First model with hidden = 2
nn_1 <- neuralnet(f, data = train, hidden = 2,linear.output = TRUE )

#Prediction

predicted_nn_1 <- compute(nn_1, test[1:4])
str(predicted_nn_1)

true.prediction <- predicted_nn_1$net.result * (max(df$Profit)-min(df$Profit))+min(df$Profit)

#Convert the test data
test.r <- (test$Profit)*(max(df$Profit)-min(df$Profit))+min(df$Profit)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_1 , rep = "best")

#Second model with hidden = 2
nn_2 <- neuralnet(f, data = train, hidden = 3,linear.output = TRUE )

#Prediction

predicted_nn_2 <- compute(nn_2, test[1:4])
str(predicted_nn_2)

true.prediction <- predicted_nn_2$net.result * (max(df$Profit)-min(df$Profit))+min(df$Profit)

#Convert the test data
test.r <- (test$Profit)*(max(df$Profit)-min(df$Profit))+min(df$Profit)
MSE.nn <- sum((test.r-true.prediction)^2)/nrow(test)
MSE.nn
error.df <- data.frame(test.r, true.prediction)
head(error.df)
plot(nn_2 , rep = "best")

#THe best model is nn_2

par(mar = numeric(4), family = 'serif')
plotnet(nn_2, alpha = 0.6)
