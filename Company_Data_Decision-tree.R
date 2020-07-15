#Setting the directory
setwd('C:\\Users\\singh\\Documents\\Decision Trees')

#Reading file
company_data <- read.csv('Company_Data.csv')
View(company_data)
summary(company_data)
str(company_data)

library(caTools)
mean(company_data$Sales)
median(company_data$Sales)
range(company_data$Sales)
boxplot(company_data$Sales, ylab = "Sales")

##Since average is 7.49, and box plot shows that 75 quatertile is at 10, we set above 10 as high 

company_data$Sales <- ifelse(company_data$Sales < 10, "low", "high")
str(company_data)
data_split <- sample.split(company_data$Sales, SplitRatio = 3/4)

sales_train <- subset(company_data, data_split == TRUE)
sales_test <- subset(company_data, data_split == FALSE)

install.packages('rpart.plot')
library(rpart.plot)
library(rpart)
company_tree <- rpart(Sales ~ . , data = sales_train, method = 'class')
company_tree
prp(company_tree)


predict_sales <- predict(company_tree, sales_test, type = 'class')
sales_accuracy <- mean(sales_test$Sales==predict_sales)
sales_accuracy
# 0.87

#Prune function Determines a nested sequence of subtrees of the supplied rpart object by recursively snipping off the least important splits, based on the complexity parameter
company_tree$cptable
#CP nsplit rel error    xerror      xstd
#1 0.15254237      0 1.0000000 1.0000000 0.1166868
#2 0.10169492      2 0.6949153 0.8983051 0.1119628
#3 0.02542373      3 0.5932203 0.7457627 0.1038565
#4 0.01694915      5 0.5423729 0.8305085 0.1085231
#5 0.01000000      7 0.5084746 0.8305085 0.1085231

Company_sales <- which.min(company_tree$cptable[, "xerror"])
company_sales_prune <- company_tree$cptable[Company_sales, 'CP']
company_sales_prune
#0.02542373

company_pruned_sales <- prune(company_tree, company_sales_prune)
prp(company_pruned_sales)

predict_sales_pruned <- predict(company_pruned_sales, sales_test, type = 'class')
pruned_accuracy <- mean(predict_sales_pruned==sales_test$Sales)
pruned_accuracy
#0.87

