#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Forecasting')

#Loading required packages
#install.packages("rmarkdown")
library(rmarkdown)
#install.packages("forecast")
library(forecast)
#install.packages("fpp")
library(fpp)
#install.packages('smooth')
library(smooth)

#reading file
airline <- read.csv('Airlines FOrecast assignment.csv')
head(airline)
plot(airline, type = "o")
plot(airline$Passengers,type="o")

#Creating dummy variables
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb
head(X)

airline_data<-cbind(airline,X)
View(airline_data)

airline_data["t"]<- 1:96
airline_data["log_Passenger"]<-log(airline_data["Passengers"])
airline_data["t_square"]<-airline_data["t"]*airline_data["t"]
attach(airline_data)

#Data Partiton
train<-airline_data[1:84,]
test<-airline_data[85:96,]

#Linear model
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear
#53.19924

#Exponential Model

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#46.05736

#Quadratic

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
#48.05189

#Additive Seasonality

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#132.8198

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#35.34896

#Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#26.36082

#Multiplicative Seasonality
multi_sea_model<-lm(log_Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
#140.0632

#Multiplicative Seasonality Linear trend
multi_add_sea_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
#10.51917

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")

# Multiplicative Seasonality Linear trend  has least RMSE value

new_model<-lm(log_Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airline_data)
new_model_pred<-data.frame(predict(new_model,newdata=airline_data,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(airline$Month)

Final <- as.data.frame(cbind(Month,airline_data$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

