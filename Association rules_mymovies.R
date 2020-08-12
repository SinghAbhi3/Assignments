#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

#Loading required packages
library(arules)
library(arulesViz)
library(rmarkdown)

mymovies <- read.csv('my_movies.csv')
View(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))

#Plots
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")

#Conclusion
#Most people have watched Gladiator, Sixth sense along with Patrioit
#Most of them has watched Lord of the rings movies along with Gladiator and Greenville

