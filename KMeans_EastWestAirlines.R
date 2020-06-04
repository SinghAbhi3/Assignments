#Setting Working Directory

setwd('C:\\Users\\singh\\Documents\\Clustering')

#Reading file

EastWestAirlines <- read.csv('EastWestAirlines.csv')
View(EastWestAirlines)
summary(EastWestAirlines)
str(EastWestAirlines)

#Scaling data
EastWestAirlines_normalized <- scale(EastWestAirlines[,2:12])

#We will now determine numbers of clusters by scree-plot

EastWestAirlines_Cluster <- (nrow(EastWestAirlines_normalized)-1)*sum(apply(EastWestAirlines_normalized, 2, var))      
for (i in 2:12) EastWestAirlines_Cluster[i] = sum(kmeans(EastWestAirlines_normalized, centers=i)$withinss)
plot(1:12, EastWestAirlines_Cluster, type='b', xlab='Number of Clusters', ylab='Within groups sum of squares')   # We can see 'elbow' like structure
title(sub = 'Scree-Plot')

fit <- kmeans(EastWestAirlines_normalized, 3) # 3 clusters
fit
EastWestAirlines_final<- data.frame(EastWestAirlines, fit$cluster) # append cluster membership
EastWestAirlines_final

#Using data manipulation function #aggregate
# final2
EastWestAirlines_Ultimate <- aggregate(EastWestAirlines[,2:12], by=list(fit$cluster), FUN=mean)
EastWestAirlines_Ultimate

table(fit$cluster)
#1    2    3 
#2574 1259  166 


#There are three clusters
#The cluster with 2574 data comes under average category
#The cluster with 1259 data comes under premium
#The cluster with 166 data comes under most premium category


