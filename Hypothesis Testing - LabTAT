#Setting working directory

setwd('C:\\Users\\singh\\Downloads\\Hypothesis testing')

####Inputs are 4 lab reports which is Discrete
####Output is continuous as we are trying to see the difference in average TAT

#####Anova one way test

LabTAT <- read.csv('LabTAT.csv')
head(LabTAT)
View(LabTAT)
attach(LabTAT)


########Normality test

shapiro.test(LabTAT$Laboratory.1)
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.3)
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution

shapiro.test(LabTAT$Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution


#####Variance test

var.test(Laboratory.1, Laboratory.2)#variance test
# p-value = 0.1675 > 0.05 so p high null fly => Equal variances

var.test(Laboratory.2, Laboratory.3)#variance test
# p-value = 0.2742 > 0.05 so p high null fly => Equal variances

var.test(Laboratory.3, Laboratory.4)#variance test
# p-value = 0.3168 > 0.05 so p high null fly => Equal variances

var.test(Laboratory.1, Laboratory.4)#variance test
# p-value = 0.1408 > 0.05 so p high null fly => Equal variances

var.test(Laboratory.2, Laboratory.4)#variance test
# p-value = 0.9261 > 0.05 so p high null fly => Equal variances


#Data Stacking

LabTAT_Stacked <- stack(LabTAT)
head(LabTAT_Stacked)


#ANOVA Test
#Ho= Average TAT for all the samples is same
#Ha= Averages TAT for all the samples is not same

LabTAT_final <- aov(values~ind,data = LabTAT_Stacked)
summary(LabTAT_final)

# p-value = 2e - 16 < 0.05. We have to reject the null hypothesis. Average TAT for atleast 1 lab is different. 





