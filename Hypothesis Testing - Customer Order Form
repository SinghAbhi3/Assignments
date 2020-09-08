#Setting working directory
setwd('C:\\Users\\singh\\Downloads\\Hypothesis testing')

#Reading file

cof <- read.csv('Costomer+OrderForm.csv')

#Converting categorical variable into 0 and 1

levels(cof$Phillippines) <- c(0,1)
levels(cof$Indonesia) <- c(0,1)
levels(cof$Malta) <- c(0,1)
levels(cof$India) <- c(0,1)

summary(cof)
cof_matrix <- matrix(c(29,33,31,20,271,267,269,280),ncol=4,byrow=TRUE)
cof_matrix
colnames(cof_matrix) <- c('Phillippines','Indonesia','Malta','India')
rownames(cof_matrix) <- c('Defective','Error Free')
cof_matrix
cof_final <- as.table(cof_matrix)

#Chi-Square test

#H0: All are same
#H1: Defective % varies by center

chisq.test(cof_final)
# p-value = 0.2771 > 0.05 so p high null fly => H0: All are same
