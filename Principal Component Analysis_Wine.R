#Setting working directory

setwd('C:\\Users\\singh\\Documents\\PCA')

#Loading required packages
library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
#install.packages('eclust')
library(eclust)

#Reading file

wine <- read.csv('wine.csv')

head(wine)
View(mywine)
summary(mywine)

#Using dplyr remoing column 1
library(dplyr)
mywine <- select(wine,-Type)
View(mywine)

#Finding Correlation
cor(mywine)

#Principal Component

mywine_pca<-princomp(mywine, cor = TRUE, scores = TRUE, covmat = NULL)
mywine_pca
summary(mywine_pca)
str(mywine_pca)
loadings(mywine_pca)

# Plot showing importance of principal components 
plot(mywine_pca)
#From the graph we can see that comp 1 is of most importance

biplot(mywine_pca)


plot(cumsum(mywine_pca$sdev*mywine_pca$sdev)*100/(sum(mywine_pca$sdev*mywine_pca$sdev)),type="b")
#The plot shows the increase of variance with considering principal components
#that helps in choosing number of principal components


# Top 3 PCA Scores which represents the whole data
mywine_pca$scores[,1:3] 


# Considering top 3 principal component scores and binding them with mydata
mywine_pca_1<-cbind(wine,mywine_pca$scores[,1:3])
View(mywine_pca_1)





#Finding number of optimal clusters

Number_of_Clusters <- NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
#According to the majority rule, the best number of clusters is  7 





# Hierarchial Clustering

#We will consider only pca scores as they represent the entire data)
wine_clustered<-mywine_pca_1[,8:10]

# Normalizing the data 
normalized_wine<-scale(wine_clustered) 
d<-dist(normalized_wine,method = "euclidean") 
#Euclidean distance

fit<-hclust(d,method="complete") 

plot(fit) 

rect.hclust(fit, k=7, border="red")

# Cutting the dendrogram for 7 clusters
wine_cut<-cutree(fit,7) 

# Cluster numbering
membership<-as.matrix(wine_cut)  

final_wine_1 <-cbind(membership,wine)
View(final_wine_1)

final_wine_HClustering <- aggregate(final_wine_1[,-c(2,16:18)],by=list(membership),FUN=mean)
View(final_wine_HClustering)






#K-Means Clustering

mywine_pca_1<-cbind(wine,mywine_pca$scores[,1:3])
ncol(mywine_pca_1)
str(mywine_pca_1)
final_wine_2 <-cbind(membership,mywine_pca_1)
View(final_wine_2)
normalized_final_wine_2 <-scale(final_wine_2[,15:17])

# Determining number of clusters by scree-plot

wss = (nrow(normalized_final_wine_2)-1)*sum(apply(normalized_final_wine_2, 2, var))     
for (i in 1:7) wss[i] = sum(kmeans(normalized_final_wine_2, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   
title(sub = "K-Means Clustering Scree-Plot")
#Number of Clusters = 7

fit_1 <- eclust(normalized_final_wine_2, "kmeans", k = 7, nstart = 25, graph = FALSE) 
fviz_cluster(fit_1, geom = "point", frame.type = "norm")

table(fit_1$cluster)
#1  2  3  4  5  6  7 
#20 24 34 30 26 19 25 

