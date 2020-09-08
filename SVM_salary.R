#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Support Vector Machine')

#Loading required packages
library(caret)
library(e1071)
library(dplyr)
library(ggplot2)
library(psych)
install.packages('kernlab')
library(kernlab)

#Reading training data file
train_salary <- read.csv('SalaryData_Train(1).csv')
str(train_salary)
train_salary_factor <- train_salary %>% mutate_if(is.character,as.factor)
str(train_salary_factor)
train_salary <- train_salary_factor
str(train_salary)
class(train_salary)
train_salary$educationno <- factor(train_salary$educationno)

#Reading test data file

test_salary <- read.csv('SalaryData_Test(1).csv')
test_salary <- test_salary %>% mutate_if(is.character, as.factor)
str(test_salary)
test_salary$educationno <- factor(test_salary$educationno)


#Visualization

vis_1 <- ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$age, fill = train_salary$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
vis_1


vis_2 <- ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$capitalloss, fill = train_salary$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot") 
vis_2 


vis_3 <- ggplot(data=train_salary,aes(x=train_salary$Salary, y = train_salary$hoursperweek, fill = train_salary$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
vis_3


#Building model

model_1 <- ksvm(train_salary$Salary~., 
             data= train_salary, kernel = "vanilladot")
model_1


Salary_prediction <- predict(model_1, test_salary)

table(Salary_prediction,test_salary$Salary)

#Salary_prediction  <=50K  >50K
#<=50K  10601  1554
#>50K     759  2146


agreement <- Salary_prediction == test_salary$Salary
table(agreement)
#agreement
#FALSE  TRUE 
#2313 12747 

prop.table(table(agreement))
#   FALSE      TRUE 
#0.1535857 0.8464143 


rfdot_model<-ksvm(train_salary$Salary~., 
                  data= train_salary,kernel = "rbfdot")
rfdot_model

pred_rfdot<-predict(rfdot_model,newdata=test_salary)
mean(pred_rfdot==test_salary$Salary)
#0.8521248


vanilla_model<-ksvm(train_salary$Salary~., 
                    data= train_salary,kernel = "vanilladot")
pred_vanilla<-predict(vanilla_model,newdata=test_salary)
mean(pred_vanilla==test_salary$Salary) 
#0.8464143

#We're getting the better result by rfdot model