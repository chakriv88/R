str(AirlinesCluster)

summary(AirlinesCluster)

#Normalise the data
library(caret)
preproc = preProcess(AirlinesCluster)
airlinesNorm = predict(preproc, AirlinesCluster)
summary(airlinesNorm)
sapply(1:7, function(x) sd(airlinesNorm[[x]])) #Standard Deviation is 1

distance <- dist(airlinesNorm, method = "euclidean")
cluster <- hclust(distance, method = "ward.D")
plot(cluster)
clusterGroups <- cutree(cluster, k = 5)
table(clusterGroups)

sapply(1:7, function(x) tapply(AirlinesCluster[[x]], clusterGroups, mean))

set.seed(88)
KMC <- kmeans(airlinesNorm, iter.max = 1000, center = 5)
table(KMC$cluster)
