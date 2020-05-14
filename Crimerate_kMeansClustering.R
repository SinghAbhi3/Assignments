#Setting working directory
setwd('C:\\Users\\singh\\Documents\\HClustering')

# Loading the data

crime_US <- read.csv('crime_data.csv')

#Removing any NA values that might be present and converting the data into a matrix.

crime0 <- na.omit(USArrests)

crime_matrix <- data.matrix (crime0)

str(crime_matrix)

crime_A <- kmeans(crime_matrix, 5)

str(crime_A)

#Distortion(withinss&betweenss)
#The distortion can be calculated in terms of  'withinss' from each of the clusters. Lesser the value of 'withinss' of a particular cluster, more densely populated it will be, thus minimum distortion.

kmeans.wss.k <- function(crime_matrix, k){
  km = kmeans(crime_matrix, k)
  return (km$tot.withinss)
}

#For k=5, withinss is 24417.02
kmeans.wss.k(crime_matrix,5)

# Increasing the value of k from 5 to 10
kmeans.wss.k(crime_matrix,10)

#It can be seen that as the value of K increases, distortion decreases

#Elbow Curve
kmeans.dis <- function(crime_matrix, maxk){
dis=(nrow(crime_matrix)-1)*sum(apply(crime_matrix,2,var))
dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime_matrix=crime_matrix)
return(dis)
}
maxk = 10
dis = kmeans.dis(crime_matrix, maxk);
plot(1:maxk, dis, type='b', xlab="Number of Clusters",
       ylab="Distortion",
       col="blue")



