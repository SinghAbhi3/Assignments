#Setting woring directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

#Loading required packages
library(arules)

groceries <- read.csv('groceries.csv',header = FALSE)
head(groceries)
groceries <- read.transactions('groceries.csv', sep=',')
summary(groceries)
inspect(groceries[1:10])
itemFrequency(groceries[, 1:4])

#Plot
itemFrequencyPlot(groceries, support = 0.078)
itemFrequencyPlot(groceries, topN = 25)
image(sample(groceries, 100))

grocery.rules <- apriori(groceries, parameter = list(support = 0.003, confidence = 0.25, minlen = 2))
grocery.rules
#set of 1771 rules 
summary(grocery.rules)
inspect(grocery.rules[1:10])
inspect(sort(grocery.rules, by = 'lift')[1:20])
#People who purchase tropical fruit, whole milk, and yogurt are nearly 5 times more likely to purchase curd (WTF is curd?) than the typical consumer. This kind of information could prove useful to a retailer who might want to use this information and stock curd next to fruit, whole, milk, and yogurt

#we can also look at the support, confidence, and lift of a specific item. Let's take a look at the transactional patterns of ham purchases.
# We can do the same for other items as well  
ham.rules <- subset(grocery.rules, items %in% 'ham')
inspect(ham.rules)


