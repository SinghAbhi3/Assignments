#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

library(arules)
book <- read.csv('book.csv')
head(book)
View(book)
class(book)

rules <- apriori(as.matrix(book), parameter = list (supp = 0.001, conf = 0.5, maxlen=3))
rules
#set of 309 rules 
inspect(head(sort(rules, by = "lift")))

rules_1 <- apriori(as.matrix(book), parameter = list(supp = 0.001, conf = 0.5))
rules_1
#set of 8314 rules 
inspect(head(sort(rules_1, by = "lift")))

rules_2 <- apriori(as.matrix(book), parameter = list(supp = 0.02, conf = 0.5,minlen =2))
rules_2
#set of 672 rules 
inspect(head(sort(rules_2, by = "lift")))

rules_3 <- apriori(as.matrix(book), parameter = list(supp = 0.01, conf = 0.25, minlen =2))
rules_3
#set of 2082 rules 
inspect(head(sort(rules_3, by = "lift")))

rules_4 <- apriori(as.matrix(book), parameter = list(supp = 0.02, conf = 0.25,minlen =2))
rules_4
#set of 850 rules 
inspect(head(sort(rules_4, by = "lift")))

rules_5 <- apriori(as.matrix(book), parameter = list(supp = 0.01, conf = 0.5, minlen =2))
rules_5
#set of 1686 rules 
inspect(head(sort(rules_5, by = "lift")))


#CHANGING MINLENGTH


rules_6 <- apriori(as.matrix(book), parameter = list(supp = 0.02, conf = 0.5,minlen =5))
rules_6
inspect(head(sort(rules_6, by = "lift")))  
#set of 186 rules 

rules_7 <- apriori(as.matrix(book), parameter = list(supp = 0.01, conf = 0.25, minlen = 5))
rules_7
#set of 938 rules 
inspect(head(sort(rules_7, by = "lift")))

rules_8 <- apriori(as.matrix(book), parameter = list(supp = 0.02, conf = 0.25,minlen =5))
rules_8
#set of 214 rules 
inspect(head(sort(rules_8, by = "lift")))


#Removing redundant rules

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
length(subsetRules) 
#267
rules <- rules[-subsetRules] 

library(arulesViz)
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")

#Similarly we can do many plots
