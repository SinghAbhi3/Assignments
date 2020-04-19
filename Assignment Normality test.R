getwd()
cars1 <- read.csv("Cars.csv")
cars1
View(cars1)
MPGcars1 <- cars1$MPG 
MPGcars1
shapiro.test(MPGcars1)
# Since p-value is greater than 0.05, we can assume the normality

waistat <- read.csv("wc-at.csv")
waistat
waist <- waistat$Waist
waist
shapiro.test(waist)
# Since p-value is less than 0.05, data set is not normally distributed

at <- waistat$AT
at
shapiro.test(at)
# Since p-value is less than 0.05, data set is not normally distributed