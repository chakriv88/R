setwd("D:\\RStudio Directory\\Kaggle\\Fraud Detection")
creditcard <- read.csv("creditcard.csv")

#Get the feel of the data
str(creditcard)
summary(creditcard)

#Average amount in fradulent cases
tapply(creditcard$Amount, creditcard$Class, mean)

#Data Visualization
par(mfrow = c(3,10))

for (i in 2:31) {
  boxplot(creditcard[i])
}

library(vioplot)

cols <- colnames(creditcard)
n_cols <- ncol(creditcard)

for (i in 2:(n_cols)) {
  vioplot(creditcard[[i]], range = 0.1 ,names = cols[i], wex = 1)
}
#Most of the variables have values concentrated around zero

#Let's find correlation between variables to deal with multicollinearity
threshold = 0.4
n_cols = ncol(creditcard)
corr_matrix = matrix(ncol = 3)

for (i in 1:(n_cols-1)) {
  p <- i + 1
  for (j in p:n_cols) {
    correlation <- cor(creditcard[[i]],creditcard[[j]])
    if(correlation>=threshold || correlation<=(-threshold)){
      corr_matrix <- rbind(corr_matrix, c(names(creditcard[i]), names(creditcard[j]), correlation))
    }
  }
}

#Only between two variables "Amount and "V2" has slightly high correlation which is -0.53

#Split the data into Train and Test set in 60:40 ratio
set.seed(144)
library(caTools)
split <- sample.split(creditcard$Class, SplitRatio = 0.6)
train <- subset(creditcard, split == TRUE)
test <- subset(creditcard, split == FALSE)

#Baseline Accuracy
table(test$Class)
BaslineAccuracy <- 113726/nrow(test) #0.99827. Our model should perform better than this.

#Let's built CART model with default parameters wihout minbucket or cp
library(rpart)
library(rpart.plot)

CART <- rpart(Class ~ ., data = train)
prp(CART)
predTest <- predict(CART, newdata = test)
table(test$Class, predTest > 0.5)
Precision <- 146/(146+14) #0.9125
Recall <- 146/(146 + 51) #0.7411
FScore <- 2*Precision*Recall/(Precision + Recall) #0.8179
Accuracy <- (146 + 113712)/nrow(test) #0.99942. Better than baseline model.

#Since cost of True Positive is more than False Negative, let's decrease our threshold value.

table(test$Class, predTest > 0.1)
Precision <- 156/(156+41) #0.7918
Recall <- 156/(156 + 41) #0.7918
FScore <- 2*Precision*Recall/(Precision + Recall) #0.7918
Accuracy <- (156+113685)/nrow(test) #0.99928. Eventhough accuracy decreased, we predicted more no of fraud cases which is the main objective.

#Find AUC value to measure how good our model is.
library(ROCR)
ROCRPred <- prediction(predTest, test$Class)
ROCRPerf <- performance(ROCRPred, "rec", "prec")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
AUC <- as.numeric(performance(ROCRPred, "auc")@y.values)
AUC #0.9059295

#Now time for Cross Validation using caret and e1071 library.
library(caret)
library(e1071)
tr.Control <- trainControl(method = "cv", number = 20)
tune.Grid <- expand.grid(.cp=seq(0,0.5,0.01))
CV <- train(Class ~ ., data = train, method = "rpart", tuneGrid = tune.Grid, trControl = tr.Control)
CV #Best model with cp value 0.01
prp(CV$finalModel)

#Build a CART model with cp = 0.01
CART <- rpart(Class ~ ., data = train, cp = 0.01)
prp(CART) #Similar to prp(CV$finalModel)
predTest <- predict(CART, newdata = test)

ROCRPred <- prediction(predTest, test$Class)
ROCRPerf <- performance(ROCRPred, "rec", "prec")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
AUC <- as.numeric(performance(ROCRPred, "auc")@y.values)
AUC #0.9059295. No change to previous score.

#Use minbucket parameter with classification method.
CART <- rpart(Class ~ ., data = train, minbucket = 20, method = "class")
prp(CART) 
predTest <- predict(CART, newdata = test, type = "class")
confusionMatrix(test$Class, predTest)
#Time to move on to another model.

#Time to try Random Forest
library(randomForest)
RF <- randomForest(Class ~ ., data = train, ntree = 200, nodesize = 20)
predTest <- predict(RF, newdata = test)
table(test$Class, predTest > 0.1)
Precision <- 164/(164+55) #0.7488
Recall <- 164/(164 + 33) #0.8324
FScore <- 2*Precision*Recall/(Precision + Recall) #0.7884
Accuracy <- (164 + 113671)/nrow(test) #0.9992 Better than baseline model.

ROCRPred <- prediction(predTest, test$Class)
ROCRPerf <- performance(ROCRPred, "rec", "prec")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
AUC <- as.numeric(performance(ROCRPred, "auc")@y.values)
AUC #0.9752674. Good increase in AUC score. May be by increasing ntree count, we may get better value.


