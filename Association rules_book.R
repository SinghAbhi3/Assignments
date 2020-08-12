#Setting working directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

#Loading required packages
library(arules)
library(arulesViz)
library(rmarkdown)

#Reading file
book <- read.csv('book.csv')
head(book)
View(book)
class(book)
rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
inspect(head(sort(rules, by = "lift")))  

head(quality(rules))

#Plot

plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")

#Conclusion

## The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other chld, art, geo, Doit books)