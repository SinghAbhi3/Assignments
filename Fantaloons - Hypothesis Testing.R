#Setting Working Directory

setwd('C:\\Users\\singh\\Downloads\\Hypothesis testing')

#Reading file
Fantaloons <- read.csv('Faltoons.csv')
head(Fantaloons)
View(Fantaloons) 
attach(Fantaloons)
summary(Fantaloons)
#   Weekdays     Weekend   
#Female:287   Female:233  
#Male  :113   Male  :167  

#creating Contingency table
table(Fantaloons)

###########Weekend
#Weekdays Female Male
#Female    167  120
#Male       66   47

#2-proportion test

#H0:% of males versus females walking in to the store during weekdays and weekend is same
#H1:% of males versus females walking in to the store during weekdays and weekend is  not same

Fantaloons_test <- prop.test(x=c(167,66),n=c(287,107),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
Fantaloons_test
# p-value = 0.5303 > 0.05 so p high null fly => Accept H0: % of males versus females walking in to the store during weekdays and weekend is same