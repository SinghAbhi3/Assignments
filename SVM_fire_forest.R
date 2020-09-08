#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Support Vector Machine')

#Installing required package
library(caret)
library(dplyr)
library(kernlab)

#Reading file
fire <- read.csv('forestfires.csv')
str(fire)
fire <- fire %>% mutate_if(is.character,as.factor)
str(fire)

hist(fire$area)
#We can see from the plot that the variable area has lots of zeros

#We will now transform the area value
fire_1 <- mutate(fire, y = log(area + 1))  
hist(fire_1$y)

#Applying normalization technique to the wole dataset

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
fire$temp = normalize(fire$temp)
fire$RH   = normalize(fire$RH)
fire$wind = normalize(fire$wind)
fire$rain = normalize(fire$rain)

attach(fire)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(fire), replace = TRUE, prob = c(0.7,0.3))
fire_train <- fire[ind==1,]
fire_test  <- fire[ind==2,]


model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= fire_train,kernel = "vanilladot")
model1

Area_pred <- predict(model1, fire_test)

table(Area_pred,fire_test$size_category)
#Area_pred large small
#large     0     0
#small    47    99

agreement <- Area_pred == fire_test$size_category
table(agreement)
#agreement
#FALSE  TRUE 
#47    99 

prop.table(table(agreement))
#agreement
#FALSE      TRUE 
#0.3219178 0.6780822 

# We will make use of different types of Kernls

model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= fire_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=fire_test)
mean(pred_rfdot==fire_test$size_category)
# 0.6849315

model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= fire_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=fire_test)
mean(pred_vanilla==fire_test$size_category) 
#0.6780822

model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= fire_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=fire_test)
mean(pred_bessel==fire_test$size_category) 
#0.6780822

model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= fire_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = fire_test)
mean(pred_poly==fire_test$size_category)
#0.6780822