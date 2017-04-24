setwd("D:\\RStudio Directory\\Edx\\Clustering\\Recitation")
healthy <- read.csv("healthy.csv", header = FALSE)
healthy <- as.matrix(healthy)
str(healthy)

image(healthy, axes = FALSE, col = grey(seq(0,1,length = 256)))

healthyVector <- as.vector(healthy)
distance <- dist(healthyVector, method = "euclidean") #Can't use hcluster due to memory issues

#K - Means algorithm
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
NumClusters = seq(2,10,1)
plot(NumClusters, SumWithinss, type = "b")
#Curve is at k = 4 or 5. So lets select k = 5

k = 5
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
healthyClusters <- KMC$cluster
KMC$centers

dim(healthyClusters) <- c(nrow(healthy), ncol(healthy))
image(healthyClusters, axes = FALSE, col = rainbow(k))

tumor <- read.csv("tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

install.packages("flexclust")
library(flexclust)

KMC.kcca <- as.kcca(KMC, healthyVector)
tumorCluster <- predict(KMC.kcca, newdata = tumorVector)
dim(tumorCluster) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorCluster, axes = FALSE, col = grey(seq(0,1,length = 256)))
image(tumorMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))
