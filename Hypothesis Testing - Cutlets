#Setting working directory

setwd('C:\\Users\\singh\\Downloads\\Hypothesis testing')

#Inputs are Diameter 1 and and Diameter 2 - Discrete 
#Output is continuous as we are trying to see the difference in diameter and diameter is a continuous variable

#Hence 2-sample t test

Cutlets <- read.csv('Cutlets.csv')
View(Cutlets)
head(Cutlets)
attach(Cutlets)

########Normality test

shapiro.test(Cutlets$Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Cutlets$Unit.B)
# p-value = 0.5255 >0.05 so p high null fly => It follows normal distribution


#####Variance test

var.test(Unit.A,Unit.B)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

##############################2 sample T Test

###Ho= Averages of diameters of Unit A is equal to Averages of diameters of unit B
###Ha= Averages of diameters of Unit A is not equal to Averages of diameters of unit B

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# p-value = 0.4723 > 0.05 so p high null fly => Averages of diameters of Unit A is equal to Averages of diameters of unit B

