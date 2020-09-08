#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

#Loading required packages
library(arules)
library(arulesViz)
library(rmarkdown)

mymovies <- read.csv('my_movies.csv')
View(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5)))
rules
#set of 77 rules 
inspect(head(sort(rules, by = "lift")))

rules_1 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5, minlen=2))) 
rules_1
inspect(head(sort(rules_1, by = "lift")))

rules_2 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.01, confidence = 0.5, minlen=2))) 
rules_2
inspect(head(sort(rules_2, by = "lift")))

rules_3 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.1, confidence = 0.25, minlen=2))) 
rules_3
inspect(head(sort(rules_3, by = "lift")))


#Changing Minlength

rules_4 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5, minlen=5))) 
rules_1
inspect(head(sort(rules_4, by = "lift")))

rules_5 <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.01, confidence = 0.5, minlen=2))) 
rules_5
inspect(head(sort(rules_5, by = "lift")))

#Plots
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")

#Similarly we can do many plots

#Removing redundant rules

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
length(subsetRules) 
#69
rules <- rules[-subsetRules] 
rules


