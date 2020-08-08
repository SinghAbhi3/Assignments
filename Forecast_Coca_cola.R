#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Forecasting')

#Loading required package
library(forecast)
library(fpp)
library(smooth)
library(rmarkdown)

#Reading file

CocaCola <- read.csv('CocaCola_forecasting.csv')
View(CocaCola)
plot(CocaCola$Sales,type="o")


Q1 <-  ifelse(grepl("Q1",CocaCola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola$Quarter),'1','0')

CocaCola_sales <- cbind(CocaCola,Q1,Q2,Q3,Q4)
View(CocaCola_sales)

CocaCola_sales["t"]<- 1:42
CocaCola_sales["log_Sales"]<-log(CocaCola_sales["Sales"])
CocaCola_sales["t_square"]<-CocaCola_sales["t"]*CocaCola_sales["t"]


#Data Partition
train<-CocaCola_sales[1:36,]
test<-CocaCola_sales[37:40,]

#Linear model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
# 644.0188

#Exponential
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
# 524.7351

#Quadratic
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
#434.7185

#Additive Seasonality
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))        
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#1785.135

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#534.6979

#Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))

rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#236.7075

#Multiplicative Seasonality
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 
# 1871.203

#Multiplicative Seasonality Linear trend

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
# 335.1026

## Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=CocaCola_sales)
new_model_pred<-data.frame(predict(new_model,newdata=CocaCola_sales,interval='predict'))
new_model_fin <- new_model$fitted.values
Quarter <- as.data.frame(CocaCola_sales$Quarter)

Final <- as.data.frame(cbind(Quarter,CocaCola_sales$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")

#aCTUAL gRAPH
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="red",type="o") 

#Predicted Graph
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Green",type="s")

