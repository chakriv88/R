table(StocksCluster$PositiveDec)
6324/(nrow(StocksCluster))
mean(StocksCluster$PositiveDec)
sort(cor(StocksCluster[,1:11]))
colMeans(StocksCluster)

library(caTools)
set.seed(144)

spl = sample.split(StocksCluster$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(StocksCluster, spl == TRUE)
stocksTest = subset(StocksCluster, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = "binomial")
summary(StocksModel)
TrainPred <- predict(StocksModel, stocksTrain, type = "response")
table(stocksTrain$PositiveDec, TrainPred > 0.5)
mean(stocksTrain$PositiveDec == (TrainPred > 0.5))

TestPred <- predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, TestPred > 0.5)
mean(stocksTest$PositiveDec == (TestPred > 0.5))

BaselineAccuracy <- mean(stocksTest$PositiveDec)

limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL

limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

library(caret)

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

colMeans(normTrain)
colMeans(normTest)

set.seed(144)
km <- kmeans(normTrain, centers = 3)
table(km$cluster)
km$size

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(km$cluster, clusterTrain)
table(clusterTest)

stocksTrainSub <- split(stocksTrain, clusterTrain)
stocksTestSub <- split(stocksTest, clusterTest)

sapply(1:3, function(i) mean(stocksTrainSub[[i]][,12]))

#StocksModelList <- sapply(1:3, function(i) glm(PositiveDec ~ ., stocksTrainSub[[i]], family = "binomial"))

#summary(StocksModelList[[4]])

StocksModel1 <- glm(PositiveDec ~ ., stocksTrainSub[[1]], family = "binomial")
summary(StocksModel1)
StocksModel2 <- glm(PositiveDec ~ ., stocksTrainSub[[2]], family = "binomial")
summary(StocksModel2)
StocksModel3 <- glm(PositiveDec ~ ., stocksTrainSub[[3]], family = "binomial")
summary(StocksModel3)

TestPred1 <- predict(StocksModel1, newdata = stocksTestSub[[1]], type = "response")
mean(stocksTestSub[[1]][,12] == (TestPred1 > 0.5))
TestPred2 <- predict(StocksModel2, newdata = stocksTestSub[[2]], type = "response")
mean(stocksTestSub[[2]][,12] == (TestPred2 > 0.5))
TestPred3 <- predict(StocksModel3, newdata = stocksTestSub[[3]], type = "response")
mean(stocksTestSub[[3]][,12] == (TestPred3 > 0.5))

AllPred <- c(TestPred1, TestPred2, TestPred3)
AllOutcomes <- c(stocksTestSub[[1]][,12], stocksTestSub[[2]][,12], stocksTestSub[[3]][,12])
mean(AllOutcomes == (AllPred > 0.5))
