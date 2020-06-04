#Setting working directory

setwd('C:\\Users\\singh\\Documents\\Clustering')

#Reading file

crimerate_k <- read.csv('crime_data.csv')
View(crimerate_k)
str(crimerate_k)
summary(crimerate_k)

#Removing any NA values that might be present and converting the data into a matrix.
any(is.na(USArrests))
crimerate_k_A<- na.omit(USArrests)
head(crimerate_k_A)

#Scaling the data
crimerate_k_B <- scale(USArrests)
summary(crimerate_k_B)


#Computing K-Means

kmeans_crimerate <- kmeans(crimerate_k_B,4, nstart = 25)
kmeans_crimerate

#Computing the mean of each of the variables in the clusters:

kmeans_crimerate_A<-aggregate(USArrests, by=list(cluster=kmeans_crimerate$cluster), mean)
kmeans_crimerate_A

#Cluster Visualizations

library('factoextra')

crimerate_final <- fviz_cluster(kmeans_crimerate, crimerate_k_B,
             palette = c('red','blue', 'green', 'orange'),
             ggtheme = theme_bw(),
             main = 'Clustering'
)

crimerate_final

#There are total 4 clusters
#Cluster 4 has the least crime rate
#Cluster 1 - maximum number of murder 
#Custer 2 - maximum number of Rape,Assault and murder Pop and second in Murder.
#Cluster 3 - 2nd in UrbanPop Crime  