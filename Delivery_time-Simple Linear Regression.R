delivery_time <- read.csv('delivery_time.csv')
View(delivery_time)
any(is.na(delivery_time$Delivery.Time))
any(is.na(delivery_time$Sorting.Time))
plot(delivery_time$Sorting.Time,delivery_time$Delivery.Time)
boxplot(delivery_time$Sorting.Time)
boxplot(delivery_time$Delivery.Time)

#Correlation
cor(delivery_time$Sorting.Time,delivery_time$Delivery.Time)

#Building model

model_A <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time)
summary(model_A)
#Residual standard error: 2.935 on 19 degrees of freedom
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#F-statistic:  40.8 on 1 and 19 DF,  p-value: 3.983e-06
confint(model_A,level = 0.95)
predict_model_A <- predict(model_A, interval='predict')
predict_model_A
predict_model_A <- as.data.frame(predict_model_A)
cor(predict_model_A$fit,delivery_time$Delivery.Time)

#Transforming the variables to check if the predicted values are better

#Square root
model_sqrt <- lm(Delivery.Time ~ sqrt(Sorting.Time), data = delivery_time)
summary(model_sqrt)
#Residual standard error: 2.872 on 19 degrees of freedom
#Multiple R-squared:  0.6958,	Adjusted R-squared:  0.6798 
#F-statistic: 43.46 on 1 and 19 DF,  p-value: 2.611e-06
confint(model_sqrt,level = 0.95)
predict_model_sqrt <- predict(model_sqrt, interval='predict')
predict_model_A
predict_model_sqrt <- as.data.frame(predict_model_sqrt)
cor(predict_model_sqrt$fit,delivery_time$Delivery.Time)


#Log
model_log <- lm(Delivery.Time ~ sqrt(Sorting.Time), data = delivery_time)
summary(model_log)
#Residual standard error: 2.872 on 19 degrees of freedom
#Multiple R-squared:  0.6958,	Adjusted R-squared:  0.6798 
#F-statistic: 43.46 on 1 and 19 DF,  p-value: 2.611e-06
confint(model_log,level = 0.95)
predict_model_log <- predict(model_log, interval='predict')
predict_model_log
predict_model_log <- as.data.frame(predict_model_log)
cor(predict_model_log$fit,delivery_time$Delivery.Time)

#Applying square root to the variable to be predicted
model_sqrt_1 <- lm(sqrt(Delivery.Time) ~ Sorting.Time, data = delivery_time)
summary(model_sqrt_1)
#Residual standard error: 0.3494 on 19 degrees of freedom
#Multiple R-squared:  0.704,	Adjusted R-squared:  0.6885 
#F-statistic:  45.2 on 1 and 19 DF,  p-value: 2.001e-06
confint(model_sqrt_1,level = 0.95)
predict_model_sqrt_1 <- predict(model_sqrt_1, interval='predict')
predict_model_sqrt_1
predict_model_sqrt_1 <- as.data.frame(predict_model_sqrt_1)
cor(predict_model_sqrt_1$fit,delivery_time$Delivery.Time)

#Applying log to the variable to be predicted
model_log_1 <- lm(log(Delivery.Time) ~ Sorting.Time, data = delivery_time)
summary(model_log_1)
#Residual standard error: 0.1755 on 19 degrees of freedom
#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 
#F-statistic: 46.73 on 1 and 19 DF,  p-value: 1.593e-06
confint(model_log_1,level = 0.95)
predict_model_log_1 <- predict(model_log_1, interval='predict')
predict_model_log_1
predict_model_log_1 <- as.data.frame(predict_model_log_1)
cor(predict_model_log_1$fit,delivery_time$Delivery.Time)

# We can clearly see that the best model to make prediction is model_log_1 with R squared value of  0.7109.
q()
