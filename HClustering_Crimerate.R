# Loading data

crimerate <- read.csv('crime_data.csv')

#Checking for NA Values
any(is.na(crimerate))

crimerate_normalizedata <- scale(crimerate[,2:5]) #normalizing

d <- dist(crimerate_normalizedata, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
plot(fit, hang=-1)

rect.hclust(fit, k=3, border="red")
groups <- cutree(fit, k=3) # cut tree into 5 clusters
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crimerate, membership)
View(final)

final_cluster_crimerate <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final_cluster_crimerate)
