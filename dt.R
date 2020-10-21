##install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(caret)

#Import dataset
data(iris)
iris

#Build Training and testing data
set.seed(2)
id <- sample(2,nrow(iris),prob=c(0.7,0.3), replace=TRUE)
print(id)
train <- iris[id==1,]
test <- iris[id==2,]
View(train)
View(test)

#Build model
fit1 <- rpart(Species ~ ., data=train, method="class",
              control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
print(fit1)
plotcp(fit1)
summary(fit1)
rpart.plot(fit1, extra=1)

#Testing the model
model_test <- predict(fit1, test, type = "class")
confusionMatrix(table(model_test, test$Species))
