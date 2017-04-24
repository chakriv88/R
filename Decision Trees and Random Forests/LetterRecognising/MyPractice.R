#Structure of the data
str(letters_ABPR)

letters_ABPR$isB <- as.factor(letters_ABPR$letter == "B")

table(letters_ABPR$letter)

set.seed(1000)
library(caTools)
split <- sample.split(letters_ABPR, SplitRatio = 0.5)
Train <- subset(letters_ABPR, split == TRUE)
Test <- subset(letters_ABPR, split == FALSE)

table(Test$isB)

BaselineAccuracy <- (1169)/1558

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=Train, method="class")
prp(CARTb)
TestPredict <- predict(CARTb, newdata = Test, type = 'class')
table(Test$isB, TestPredict)
Accuracy <- (1136+319)/(nrow(Test))

library(randomForest)
RandomF <- randomForest(isB ~ . - letter, data = Train, nodesize = 0.1, ntree = 200, method = "rpart")
RFTestPredict <- predict(RandomF, newdata = Test, type = 'class')
table(Test$isB, RFTestPredict)
Accuracy <- (1162+365)/nrow(Test)

letters_ABPR$letter <- as.factor(letters_ABPR$letter)

set.seed(2000)
split <- sample.split(letters_ABPR$letter, SplitRatio = 0.5)
Train <- subset(letters_ABPR, split == TRUE)
Test <- subset(letters_ABPR, split == FALSE)

table(Test$letter)

BaselineAccuracy <- 401/nrow(Test)

CARTb = rpart(letter ~ . - isB, data=Train, method="class")
prp(CARTb)
TestPredict <- predict(CARTb, newdata = Test, type = 'class')
table(Test$letter, TestPredict)
Accuracy <- (348+318+363+340)/nrow(Test)

set.seed(1000)
RandomF <- randomForest(letter ~ . -isB, data = Train)
RFTestPredict <- predict(RandomF, newdata = Test)
table(Test$letter, RFTestPredict)
Accuracy <- (391+380+393+365)/nrow(Test)
mean(predict(RandomF, newdata=Test)==Test$letter)