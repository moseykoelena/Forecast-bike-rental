#1. Load the dataset bike.csvPreview the document into memory. Then split the data into a training set 
#containing 2/3 of the original data (test set containing remaining 1/3 of the original data).

library(readr)
Bike <- read_csv("Desktop/predictive modeling/Project 5/Bike.csv")
View(Bike)

trainIndex = sample(1:nrow(Bike), nrow(Bike)*2/3)
train = Bike[+trainIndex, ]
test = Bike[-trainIndex,]
View(train)
View(test)


#2. Build a tree model using function tree().
#a. The response is count and the predictors are season, holiday, workingday, temp, atemp, humidity, windspeed, casual, and registered.
library(tree)
tree.bike = tree(count~season+holiday+workingday+temp+atemp+humidity+windspeed+casual+registered, data=train)
summary(tree.bike)
#b. Perform cross-validation to choose the best tree by calling cv.tree().
plot(tree.bike)
text(tree.bike, pretty=0)
cv.bike=cv.tree(tree.bike)

#c. Plot the model results of b) and determine the best size of the optimal tree.
plot(cv.bike$size, cv.bike$dev, ttpe='b')
#Test error at the size 8 is smallest but we prefer the simple model so we choose size 4.

#d. Prune the tree by calling prune.tree() function with the best size found in c).
prune.bike=prune.tree(tree.bike, best=4)

#e. Plot the best tree model.

plot(prune.bike)
text(prune.bike, pretty=0)
#f. Compute the test error using the test data set.

yhat=predict(prune.bike, newdata=test)
mean((yhat-test$count)^2)

#3. Build a random forest model using function randomForest()
#a. The response is count and the predictors are season, holiday, workingday, temp, atemp, humidity, windspeed, casual, and registered.
library(randomForest)
rf.bike=randomForest(count~season+holiday+workingday+temp+atemp+humidity+windspeed+casual+registered,data=train, importance=TRUE)
#b. Compute the test error using the test data set.
yhat.rf=predict(rf.bike, newdata=test)
mean((yhat.rf - test$count)^2)

#c. Extract variable importance measure using importance() function.
importance(rf.bike)
#d. Plot the variable importance using function varImpPlot(). Which are the top 2 important predictors in this model?
varImpPlot(rf.bike)

#The top 2 important predictors are registered and casual
  