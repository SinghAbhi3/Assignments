getwd()
setwd('C:\\Users\\singh\\Documents\\Calories Consumed -SLR')
Calories <- read.csv('calories_consumed.csv')
head(Calories)
class(Calories)
summary(Calories)
library(lattice)
any(is.na(Calories$Weight.gained..grams.))
any(is.na(Calories$Calories.Consumed))

dotplot(Calories$Weight.gained..grams.,main='Dot Plot')
dotplot(Calories$Calories.Consumed,main='Dot Plot')

boxplot(Calories$Weight.gained..grams.,main='Dot Plot')
boxplot(Calories$Calories.Consumed,main='Dot Plot')

hist(Calories$Weight.gained..grams.)
hist(Calories$Calories.Consumed)

qqnorm(Calories$Weight.gained..grams.)
qqline(Calories$Weight.gained..grams.)
qqnorm(Calories$Calories.Consumed)
qqline(Calories$Calories.Consumed)

hist(Calories$Weight.gained..grams., prob=TRUE)
hist(Calories$Calories.Consumed, prob=TRUE)

#ScatterPlot
plot(Calories$Calories.Consumed,Calories$Weight.gained..grams.,col='dodgerblue')

#Correlation
cor(Calories$Calories.Consumed,Calories$Weight.gained..grams.)

#Building model
model_A <- lm( Weight.gained..grams. ~ Calories.Consumed, data=Calories)
summary(model_A)
#Residual standard error: 111.6 on 12 degrees of freedom
#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
#F-statistic: 104.3 on 1 and 12 DF,  p-value: 2.856e-07
confint(model_A,level = 0.95)
predict_model_A <- predict(model_A, interval='predict')
predict_model_A
predict_model_A <- as.data.frame(predict_model_A)
cor(predict_model_A$fit,Calories$Weight.gained..grams.)

#Transforming the variables to check if the predicted values are better

#Square root
model_sqrt <- lm(Weight.gained..grams. ~ sqrt(Calories.Consumed), data=Calories)
summary(model_sqrt)
#Residual standard error: 131.5 on 12 degrees of freedom
#Multiple R-squared:  0.8567,	Adjusted R-squared:  0.8448 
#F-statistic: 71.76 on 1 and 12 DF,  p-value: 2.083e-06
confint(model_sqrt,level = 0.95)
predict_model_sqrt <- predict(model_sqrt, interval='predict')
predict_model_sqrt
predict_model_sqrt <- as.data.frame(predict_model_sqrt)
cor(predict_model_sqrt$fit,Calories$Weight.gained..grams.)

#Log
model_log <- lm(Weight.gained..grams. ~ log(Calories.Consumed), data=Calories)
summary(model_log)
#Residual standard error: 152.3 on 12 degrees of freedom
#Multiple R-squared:  0.8077,	Adjusted R-squared:  0.7917 
#F-statistic:  50.4 on 1 and 12 DF,  p-value: 1.248e-05
confint(model_log,level = 0.95)
predict_model_log <- predict(model_log, interval='predict')
predict_model_log
predict_model_log <- as.data.frame(predict_model_log)
cor(predict_model_log$fit,Calories$Weight.gained..grams.)

#Applying square root to the variable to be predicted
model_sqrt_1 <- lm(sqrt(Weight.gained..grams.) ~ Calories.Consumed, data=Calories)
summary(model_sqrt_1)
#Residual standard error: 2.496 on 12 degrees of freedom
#Multiple R-squared:  0.9139,	Adjusted R-squared:  0.9067 
#F-statistic: 127.3 on 1 and 12 DF,  p-value: 9.56e-08
confint(model_sqrt_1,level = 0.95)
predict_model_sqrt_1 <- predict(model_sqrt_1, interval='predict')
predict_model_sqrt_1
predict_model_sqrt_1 <- as.data.frame(predict_model_sqrt_1)
cor(predict_model_sqrt_1$fit,Calories$Weight.gained..grams.)

#Applying log to the variable to be predicted
model_log_1 <- lm(log(Weight.gained..grams.) ~ Calories.Consumed, data = Calories)
summary(model_log_1)
#Residual standard error: 0.3314 on 12 degrees of freedom
#Multiple R-squared:  0.8776,	Adjusted R-squared:  0.8674 
#F-statistic: 86.04 on 1 and 12 DF,  p-value: 8.018e-07
confint(model_log_1,level = 0.95)
predict_model_log_1 <- predict(model_log_1, interval='predict')
predict_model_log_1
predict_model_log_1 <- as.data.frame(predict_model_log_1)
cor(predict_model_log_1$fit,Calories$Weight.gained..grams.)

# We can clearly see that the best model to make prediction about the weight gained is model_sqrt_1 with R squared value of 0.9139 . 