# Setting Working Directory

setwd('C:\\Users\\singh\\Documents\\Clustering')

# Loading required packages

install.packages('ggplot2')
library(ggplot2)

install.packages('NbClust')
library(NbClust)

install.packages('rmarkdown')
library(rmarkdown)

install.packages('rJava')
library(rJava)

library(cluster)
library(fpc)
library(dendextend)

#Reading file

EastWestAirlines <- read.csv('EastWestAirlines.csv')
View(EastWestAirlines)
head(EastWestAirlines)
attach(EastWestAirlines)
summary(EastWestAirlines)

# Normalizing data
# Excluding ID
normalized_data<-scale(EastWestAirlines[,2:12])

# Distance matrix
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="ward.D2")

plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=3, border="red")


groups <- cutree(fit, k=3) # cut tree into 3 clusters

table(groups)

# groups or cluster numbers
membership<-as.matrix(groups) 

final <- data.frame(EastWestAirlines, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

final1

#CONCLUSION

#Cluster1 - Least Priority Customers
#Cluster2 - Second most priority customer 
#Cluster3 - Premium Customers