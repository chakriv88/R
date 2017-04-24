#see the structure of the data
str(wine)

#To get the feel of the data
summary(wine)

#liner model with only one variable AGST
model1 <- lm(Price ~ AGST, data = wine)

#Summary of the model
summary(model1)

#Residuals of the model
model1$residuals

#Sum of Squared Errors of the model
SSE <- sum(model1$residuals^2)

#Baseline of the data. Our model should be able to perform better than the baseline model
Baseline <- mean(wine$Price)

#Total Sum of Squares.
SST <- sum((wine$Price-Baseline)^2)

#Calculate RSquare. If Rsquare is close to 1, then our model is performing well.
RSquare <- 1-(SSE/SST)

#Another model with two independent variables
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)

#Repeat the steps to check how good our model2 is.
summary(model2)
SSE <- sum(model2$residuals^2)
SSE

#Few more models with different set of independent varibles.
model3 <- lm( Price ~ AGST + HarvestRain + Age + FrancePop + WinterRain, data = wine)
summary(model3)
SSE <- sum(model3$residuals^2)
model4 <- lm( Price ~ HarvestRain + WinterRain, data = wine)
summary(model4)
model5 <- lm( Price ~ AGST + HarvestRain + Age + WinterRain, data = wine)
summary(model5)

#see the correlation between independent variables to find multicollinearity. If two variables are
#higly correlated, we should remove one variable.
cor(wine$FrancePop,wine$Age)
cor(wine$WinterRain, wine$Price)
cor(wine) #to check correlation between all the varibales in the dataset.

#make predictions with our best model and find RSquare.
predictTest <- predict(model5, newdata = wine_test)
SSE <- sum((predictTest - wine_test$Price)^2)
Baseline <- mean(wine$Price)
SST <- sum((Baseline - wine_test$Price)^2)
RSquare <- 1 - (SSE/SST)