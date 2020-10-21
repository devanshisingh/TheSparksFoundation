df <- read.csv(url("http://bit.ly/w-data"))
View(df)

#Data Visualisation using Scatter plot
plot(df$Hours, df$Scores, main="Scores vs Hours",
     xlab="Hours", ylab="Scores", pch=19) 

#Build Training and testing data
set.seed(2)
id <- sample(2,nrow(df),prob=c(0.7,0.3), replace=TRUE)
print(id)
train <- df[id==1,]
test <- df[id==2,]
View(train)
View(test)

#Train the model
library(e1071)
mdl <- lm(data = train, formula = Scores~Hours)
summary(mdl)
plot(df$Hours, df$Scores, col = "red", main = "Scores vs Hours", abline(mdl), 
     cex=1.2, pch = 16, ylab = "Scores", xlab = "Hours")

#Test the model
y_pred <- predict(mdl, test)
print(data.frame(y_pred, test$Scores))
library("ie2misc")
print(mae(test$Scores, y_pred))

#Predict the value at 9.25
predicted_value <- predict(mdl, data.frame(Hours=c(9.25)))
print(predicted_value)