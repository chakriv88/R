table(ClaimsData$bucket2008)

summary(ClaimsData$reimbursement2008)

#Structure of the data
str(ClaimsData)

library(caTools)
set.seed(88)
#Split into train and test
split <- sample.split(ClaimsData$bucket2009, SplitRatio = 0.6)
ClaimsTrain <- subset(ClaimsData, split == TRUE)
ClaimsTest <- subset(ClaimsData, split == FALSE)

summary(ClaimsData$age)
table(ClaimsData$diabetes)
#174254/(174254+283751)
table(ClaimsTrain$bucket2009, ClaimsTrain$bucket2008)
BaselineAccuracyTrain <- (164967 + 16172 + 4004 + 2399 + 141)/(274803)
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
BaselineAccuracyTest <- (110138 + 10721 + 2774 + 1539 + 104)/(183202)
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix
sum(PenaltyMatrix*table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))/nrow(ClaimsTest)

#Decision Tree
library(rpart)
library(rpart.plot)
ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)
predictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, predictTest)
TreeAccuracy <- (114141+16102+118+201+0)/nrow(ClaimsTest)
sum(PenaltyMatrix*table(ClaimsTest$bucket2009, predictTest))/nrow(ClaimsTest)

ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms = list(loss=PenaltyMatrix))
predictTest <- predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, predictTest)
TreeAccuracy <- (94310+18942+4692+636+2)/nrow(ClaimsTest)
sum(PenaltyMatrix*table(ClaimsTest$bucket2009, predictTest))/nrow(ClaimsTest)
