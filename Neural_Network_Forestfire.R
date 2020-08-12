#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Neural Network')

#Loading required packages
library(neuralnet)
install.packages('corrplot')
library(corrplot)

forest_data <- read.csv('forestfires.csv')
View(forest_data)
forestdata <- forest_data[-c(12:31)]
View(forestdata)
forestdata$month <- as.numeric(as.factor(forestdata$month))
forestdata$RH <- as.numeric(forestdata$RH)
forestdata$day <- as.numeric(as.factor(forestdata$day))
summary(forestdata)
str(forestdata)

par(mfrow=c(2,5))
for(j in 1:10){
  plot(density(forestdata[,j]), main = names(forestdata[j]), col = 'blue')}

c <- cor(forestdata)
par(mfrow=c(1,1))
corrplot(c, method = "color", outline = TRUE, type = 'lower')

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

norm_forest <- as.data.frame(lapply(forestdata, FUN = normalize))

splitdata <- sample.split(norm_forest, SplitRatio = 4/5)
train_forestfire <- subset(norm_forest, splitdata == TRUE)         
test_forestfire <- subset(norm_forest, splitdata == FALSE)

forestfire_neural <- neuralnet(area ~ ., data = train_forestfire)
plot(forestfire_neural)

pred_forestfire <- predict(forestfire_neural, test_forestfire)
cor(pred_forestfire, test_forestfire$area)

forestfire_neural_2 <- neuralnet(area ~ ., data = train_forestfire, hidden = 2)
plot(forestfire_neural_2)

pred_forestfire_2 <- predict(forestfire_neural_2, test_forestfire)
cor(pred_forestfire_2, test_forestfire$area)

