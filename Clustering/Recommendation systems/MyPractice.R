getwd()
setwd("D:\\RStudio Directory\\Clustering\\Recommendation systems")
movies <- read.table("http://files.grouplens.org/datasets/movielens/ml-100k/u.item", 
                     header = FALSE, quote = "\"", sep = "|")
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB",
                      "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime",
                      "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", 
                      "Romance", "SciFi", "Thriller", "War", "Western")
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

distance <- dist(movies[,2:20], method = "euclidean")
clusterMovies = hclust(distance, method = "ward.D")
plot(clusterMovies)
clusterGroups <- cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)

eval <- matrix(ncol = 10, nrow = 20)
for (i in 3:20) {
  eval[i,] <- tapply(movies[,i], clusterGroups, mean)
}

eval <- eval[3:20,]

row.names(eval) <- colnames(movies[,3:20])
colnames(eval) <- c(paste("Cluster", seq(1:10)))

subset(movies, Title == "Men in Black (1997)") #it is in 257 row. Let's see in which cluster it is
clusterGroups[257] #It is in cluster 2

#Lets see what other movies are there in cluster 2 that we can recommend to a user
length(movies$Title[clusterGroups == 2]) #199 movies

#We can recommend following movies
head(movies$Title[clusterGroups == 2], 10)
