#Structure of the data
str(census)

set.seed(2000)
library(caTools)
split <- sample.split(census$over50k, SplitRatio = 0.6)
Train <- subset(census, split == TRUE)
Test <- subset(census, split == FALSE)

#Logistic Regression
censusLog <- glm(over50k ~ ., data = Train, family = 'binomial')
summary(censusLog)
TestPredict <- predict(censusLog, newdata = Test, type = 'response')
table(Test$over50k, TestPredict > 0.5)
Accuracy = (9051+1888)/nrow(Test)
table(Test$over50k)
BaselineAccuracy <- 9713/nrow(Test)

#ROCR Curve
library(ROCR)
library(e1071)
ROCRpred <- prediction(TestPredict, Test$over50k)
ROCRPerf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRPerf)
ROCRPerf1 <- performance(ROCRpred, "prec", "rec")
plot(ROCRPerf1)
ROCRPerf2 <- performance(ROCRpred, "sens", "spec")
plot(ROCRPerf2)
AUC <- as.numeric(performance(ROCRpred, 'auc')@y.values)

#Decision Tree
library(rpart)
library(rpart.plot)
censusCART <- rpart(over50k ~ ., data = Train, method = "class")
prp(censusCART)
TestPredict1 <- predict(censusCART, newdata = Test, type = 'class')
table(Test$over50k, TestPredict1 )
Accuracy <- (9243+1596)/nrow(Test)

TestPredict1 <- predict(censusCART, newdata = Test)
ROCRpred <- prediction(TestPredict1[,2], Test$over50k)
ROCRPerf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRPerf)
ROCRPerf1 <- performance(ROCRpred, "prec", "rec")
plot(ROCRPerf1)
ROCRPerf2 <- performance(ROCRpred, "sens", "spec")
plot(ROCRPerf2)
AUC <- as.numeric(performance(ROCRpred, 'auc')@y.values)

#Random Forest
library(randomForest)
censusRF <- randomForest(over50k ~ ., data = Train)
TestPredict2 <- predict(censusRF, newdata = Test)
table(Test$over50k, TestPredict2)
Accuracy <- (9686+868)/nrow(Test)

vu = varUsed(censusRF, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusRF$forest$xlevels[vusorted$ix]))

varImpPlot(censusRF)

set.seed(2)
library(caret)
library(e1071)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr.Control <- trainControl(method = "cv", number = 10)
TrainCVTree <- train(over50k ~ ., data = Train, method = 'rpart', tuneGrid = cartGrid, trControl = tr.Control)
prp(TrainCVTree$finalModel)
TestPredict3 <- predict(TrainCVTree, newdata = Test)
table(Test$over50k, TestPredict3)
Accuracy <- (9167+1821)/nrow(Test)
