library(titanic)

require(rpart)
str(Titanic)
titanic_rpart <- rpart(Survived ~., data = Titanic)

plot(titanic_rpart) 
text(titanic_rpart)
rpart.plot(titanic_rpart)

require(party)
treeTitanic <- ctree(Survived ~., data=Titanic)
plot(treeTitanic)

cforest(Survived ~., data=Titanic, controls=cforest_control(mtry=2, mincriterion=0))

help(hclust)
dist_mat <- dist(Titanic, method = 'euclidean')
dist_mat

set.seed(240)
h_cl <- hclust(dist_mat, method="average")
h_cl

plot(h_cl)

#random forest
require(randomForest)
fitT <- randomForest(Survived ~., data=Titanic)
print(fitT)
importance(fitT)
varImpPlot(fitT)
plot(fitT)
getTree(fitT, 1, labelVar=TRUE)
