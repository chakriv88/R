distance <- dist(dailykos, method = "euclidean")

#Hierarchical clustering
ClusterDailyKos <- hclust(distance, method = "ward.D")
plot(ClusterDailyKos)

clusterGroups <- cutree(ClusterDailyKos, k = 7)
Cluster1 <- subset(dailykos, clusterGroups == 1)
Cluster2 <- subset(dailykos, clusterGroups == 2)
Cluster3 <- subset(dailykos, clusterGroups == 3)
Cluster4 <- subset(dailykos, clusterGroups == 4)
Cluster5 <- subset(dailykos, clusterGroups == 5)
Cluster6 <- subset(dailykos, clusterGroups == 6)
Cluster7 <- subset(dailykos, clusterGroups == 7)

##Alternative method
Clusters <- split(dailykos, clusterGroups)
str(Clusters[[1]])

tail(sort(colMeans(Cluster1)))
tail(sort(colMeans(Cluster2)))
tail(sort(colMeans(Cluster3)))
tail(sort(colMeans(Cluster4)))
tail(sort(colMeans(Cluster5)))
tail(sort(colMeans(Cluster6)))
tail(sort(colMeans(Cluster7)))

#Alternative method
topWords<-function(x){tail(sort(colMeans(x)))}
lapply(Clusters, topWords)

set.seed(1000)
KMC <- kmeans(dailykos, centers = 7)
str(KMC)
KMeansClusters <- split(dailykos, KMC$cluster)
nrow(KMeansClusters[[1]])
nrow(KMeansClusters[[2]])
nrow(KMeansClusters[[3]])
nrow(KMeansClusters[[4]])
nrow(KMeansClusters[[5]])
nrow(KMeansClusters[[6]])
nrow(KMeansClusters[[7]])

#Alternative Method
sapply(1:7, function(i) nrow(KMeansClusters[[i]]))

tail(sort(colMeans(KMeansClusters[[1]])))
tail(sort(colMeans(KMeansClusters[[2]])))
tail(sort(colMeans(KMeansClusters[[3]])))
tail(sort(colMeans(KMeansClusters[[4]])))
tail(sort(colMeans(KMeansClusters[[5]])))
tail(sort(colMeans(KMeansClusters[[6]])))
tail(sort(colMeans(KMeansClusters[[7]])))

table(clusterGroups, KMC$cluster)
