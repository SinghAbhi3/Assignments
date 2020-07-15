#Reading file
fc <- read.csv('Fraud_check.csv')
summary(fc)
str(fc)
head(fc)
TI = ifelse(fc$Taxable.Income<= 30000, "Risky", "Good")
head(fc)
fc_df<-data.frame(fc,TI)
fc_df
str(fc_df)
fc_df$Undergrad <- as.factor(fc_df$Undergrad)
fc_df$Marital.Status <- as.factor(fc_df$Marital.Status)
fc_df$Urban <- as.factor(fc_df$Urban)
fc_df$TI <- as.factor(fc_df$TI)
str(fc_df)
table(fc_df$TI)
#Good Risky 
#476   124 

#LOading required functions
library(MASS)
library(caret)
library(randomForest)

#Data Partition

set.seed(123)
ind <- sample(2, nrow(fc_df), replace = TRUE, prob = c(0.7,0.3))
train <- fc_df[ind==1,]
test  <- fc_df[ind==2,]
set.seed(213)
rf <- randomForest(TI~., data=train)
rf 

attributes(rf)
#$names
#[1] "call"            "type"            "predicted"       "err.rate"        "confusion"       "votes"          
#[7] "oob.times"       "classes"         "importance"      "importanceSD"    "localImportance" "proximity"      
#[13] "ntree"           "mtry"            "forest"          "y"               "test"            "inbag"          
#[19] "terms"          

#$class
#[1] "randomForest.formula" "randomForest"  

# Prediction and Confusion Matrix for Training data 
predict1 <- predict(rf, train)
head(predict1)

head(train$TI)

confusionMatrix(predict1, train$TI) 
#100% Accuracy

#Reference
#Prediction Good Risky
#Good   344     0
#Risky    0    80

#Accuracy : 1          
#95% CI : (0.9913, 1)
#No Information Rate : 0.8113     
#P-Value [Acc > NIR] : < 2.2e-16  

#Kappa : 1          

#Mcnemar's Test P-Value : NA         
                                     
 #           Sensitivity : 1.0000     
  #          Specificity : 1.0000     
   #      Pos Pred Value : 1.0000     
    #     Neg Pred Value : 1.0000     
     #        Prevalence : 0.8113     
      #   Detection Rate : 0.8113     
   #Detection Prevalence : 0.8113     
    #  Balanced Accuracy : 1.0000     
                                     
     #  'Positive' Class : Good    

#Prediction - Test data

predict2 <- predict(rf, test)
confusionMatrix(predict2, test$TI)

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction Good Risky
##      Good   132     0
##      Risky    0    44
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9793, 1)
##     No Information Rate : 0.75       
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.00       
##             Specificity : 1.00       
##          Pos Pred Value : 1.00       
##          Neg Pred Value : 1.00       
##              Prevalence : 0.75       
##          Detection Rate : 0.75       
##    Detection Prevalence : 0.75       
##       Balanced Accuracy : 1.00       
##                                      
##        'Positive' Class : Good   

plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
## mtry = 2  OOB error = 43.16% 
## Searching left ...
## mtry = 4     OOB error = 44.1% 
## -0.02185792 0.05 
## Searching right ...
## mtry = 1     OOB error = 43.16% 
## 0 0.05

rf_1 <- randomForest(TI~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf_1
## 
## Call:
##  randomForest(formula = Risky_Good ~ ., data = train, ntree = 200,      mtry = 2, importance = TRUE, proximity = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 200
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.24%
## Confusion matrix:
##       Good Risky class.error
## Good   343     1 0.002906977

tune_predict1 <- predict(rf_1, train)
confusionMatrix(tune_predict1, train$TI)

# 100 % accuracy on training data 

# test data prediction using the Tuned RF1 model
predict2 <- predict(rf_1, test)
confusionMatrix(predict2, test$TI) # 100 % accuracy on test data 
#Confusion Matrix and Statistics

#Reference
#Prediction Good Risky
#Good   132     0
#Risky    0    44

#Accuracy : 1          
#95% CI : (0.9793, 1)
#No Information Rate : 0.75       
#P-Value [Acc > NIR] : < 2.2e-16  

#Kappa : 1          

#Mcnemar's Test P-Value : NA         
                                     
#            Sensitivity : 1.00       
 #           Specificity : 1.00       
  #       Pos Pred Value : 1.00       
    #     Neg Pred Value : 1.00       
     #        Prevalence : 0.75       
      #   Detection Rate : 0.75       
  # Detection Prevalence : 0.75       
   #   Balanced Accuracy : 1.00       
                                     
    #   'Positive' Class : Good       
                             
#Number of Nodes

hist(treesize(rf_1), main = "No of Nodes for the trees", col = "orange")
varImpPlot(rf_1)
importance(rf_1)
