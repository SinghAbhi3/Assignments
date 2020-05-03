getwd()
mydata <- read.csv('ToyotaCorolla.csv')
head(mydata)
library(dplyr)
Corolla <- select(mydata, c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"))
head(Corolla,10)
Corolla
mymodel1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla)
summary(mymodel1)
#Residual standard error: 1342 on 1427 degrees of freedom
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 
#F-statistic:  1131 on 8 and 1427 DF,  p-value: < 2.2e-16
mymodel2 <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data=Corolla)
summary(mymodel2)
#Residual standard error: 1342 on 1429 degrees of freedom
#Multiple R-squared:  0.8636,	Adjusted R-squared:  0.863 
#F-statistic:  1508 on 6 and 1429 DF,  p-value: < 2.2e-16
mymodel3 <- lm(Price~Age_08_04+KM+HP+Weight, data=Corolla)
summary(mymodel3)
#Residual standard error: 1350 on 1431 degrees of freedom
#Multiple R-squared:  0.8618,	Adjusted R-squared:  0.8614 
#F-statistic:  2230 on 4 and 1431 DF,  p-value: < 2.2e-16
mymodel4 <- lm(Price~Age_08_04, data=Corolla)
summary(mymodel4)
#Residual standard error: 1746 on 1434 degrees of freedom
#Multiple R-squared:  0.7684,	Adjusted R-squared:  0.7682 
#F-statistic:  4758 on 1 and 1434 DF,  p-value: < 2.2e-16
mymodel5 <- lm(Price~KM+HP, data=Corolla)
summary(mymodel5)
library(car)

influence.measures(mymodel1)
influencePlot(mymodel1,id.n=3)

myfinalmodel1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-c(81,222,961),])
summary(myfinalmodel1)
#Residual standard error: 1231 on 1424 degrees of freedom
#Multiple R-squared:  0.8852,	Adjusted R-squared:  0.8845 
#F-statistic:  1372 on 8 and 1424 DF,  p-value: < 2.2e-16

myfinalmodel2 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-81,])
summary(myfinalmodel2)
#Residual standard error: 1313 on 1426 degrees of freedom
#Multiple R-squared:  0.8694,	Adjusted R-squared:  0.8686 
#F-statistic:  1186 on 8 and 1426 DF,  p-value: < 2.2e-16

myfinalmodel3 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-c(81,222),])
summary(myfinalmodel3)

#Residual standard error: 1270 on 1425 degrees of freedom
#Multiple R-squared:  0.8778,	Adjusted R-squared:  0.8772 
#F-statistic:  1280 on 8 and 1425 DF,  p-value: < 2.2e-16

myfinalmodel4 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-c(81,961),])
summary(myfinalmodel4)
#Residual standard error: 1291 on 1425 degrees of freedom
#Multiple R-squared:  0.8736,	Adjusted R-squared:  0.8729 
#F-statistic:  1231 on 8 and 1425 DF,  p-value: < 2.2e-16

myfinalmodel5 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-c(222,961),])
summary(myfinalmodel5)

#Residual standard error: 1295 on 1425 degrees of freedom
#Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8728 
#F-statistic:  1230 on 8 and 1425 DF,  p-value: < 2.2e-16

myfinalprediction <- predict(myfinalmodel1, interval = 'predict')
myfinalprediction
myfinalprediction <- data.frame(myfinalprediction)


# THE BEST MODEL FOR PREDICTION IS myfinalmodel1 WITH R-SQUARED VALUE OF 0.8852.


