#Setting working directory

setwd('C:\\Users\\singh\\Downloads\\Hypothesis testing')

#Reading file

BuyerRatio<- read.csv('BuyerRatio.csv')
View(BuyerRatio)
attach(BuyerRatio)

#Creating Matrix from the data
BuyerRatio_Matrix <- matrix(c(50,142,131,70,435,1523,1356,750),ncol=4,byrow=TRUE)
BuyerRatio_Matrix

#Renaming columns and rows
colnames(BuyerRatio_Matrix) <- c('East','West','North','South')
rownames(BuyerRatio_Matrix) <- c('Males','Females')

BuyerRatio_Matrix <- as.table(BuyerRatio_Matrix)

BuyerRatio_Matrix
class(BuyerRatio_Matrix)

# Chi-square test

#H0:male-female buyer rations are similar 
#H1:male-female buyer rations are not similar 
chisq.test(BuyerRatio_Matrix)
## p-value = 0.6603 > 0.05 so p high null fly =>Accept H0: male-female buyer rations are similar 
