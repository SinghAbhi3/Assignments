#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Decision Trees')

#Reading file
fraud_check <- read.csv('Fraud_check.csv')
fraud_check <- as.data.frame(fraud_check)
library(caTools)
library(rpart)
install.packages('fancyRpartPlot')
condition_fraud_check <-  ifelse(fraud_check$Taxable.Income <= 30000, 'risky','good')
fraud_check_df <- data.frame(fraud_check,condition_fraud_check) 
head(fraud_check_df,6)
split <- sample.split(fraud_check_df$condition_fraud_check, SplitRatio = 3/4)
train_fraud_check_df <- subset(fraud_check_df, split == TRUE)
test_fraud_check_df <- subset(fraud_check_df, split == FALSE)

fraud_tree<-rpart(condition_fraud_check~ Undergrad + Marital.Status + City.Population + 
                    Work.Experience + Urban + Taxable.Income, data = train_fraud_check_df)

library(rpart.plot)
prp(fraud_tree)
summary(fraud_tree)

predict_test <- predict(fraud_tree, test_fraud_check_df, type = 'class')

table(predict_test, test_fraud_check_df$condition_fraud_check)
#predict_test good risky
#good   119     0
#risky    0    31

accuracy_fraud <- mean(predict_test==test_fraud_check_df$condition_fraud_check)
accuracy_fraud
#1

#Prune

pruned <- printcp(fraud_tree)
pruned_1 <- which.min(pruned[,"xerror"])
pruned_fraud <- fraud_tree$cptable[pruned_1, 'CP']
pruned_fraud
pruned_model_fraud <- prune(fraud_tree, pruned_fraud)
pruned_model_fraud
prp(pruned_model_fraud)
