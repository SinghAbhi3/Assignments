#Setting Working Directory

setwd('C:\\Users\\singh\\Documents\\Clustering')

#Reading file

crime_rate <- read.csv('crime_data.csv')
View(crime_rate)
head(crime_rate)
attach(crime_rate)
summary(crime_rate)

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


#Normalizing the data

normalized_data<-scale(crime_rate[,2:5]) 

#Distance Matrix

d <- dist(normalized_data, method = "euclidean")

fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=4, border="red")

#4 clusters
groups <- cutree(fit, k=4) 

table(groups)

# groups or cluster numbers
membership<-as.matrix(groups) 

final <- data.frame(crime_rate, membership)
final

final_crime_rate <- final[,c(ncol(final),1:(ncol(final)-1))]
final_crime_rate

#CONCLUSION

#Cluster 4 has the least crime rate
#Cluster 1 - maximum number of murder 
#Custer 2 - maximum number of Rape,Assault and murder Pop and second in Murder.
#Cluster 3 - 2nd in UrbanPop Crime  
