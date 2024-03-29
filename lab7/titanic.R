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

#resource used: https://www.geeksforgeeks.org/hierarchical-clustering-in-r-programming/
help(hclust)
dist_mat <- dist(Titanic, method = 'euclidean')
dist_mat

set.seed(240)
h_cl <- hclust(dist_mat, method="average")
h_cl

plot(h_cl)
