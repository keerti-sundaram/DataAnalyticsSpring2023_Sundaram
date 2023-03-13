data(swiss)
library(ISLR)
View(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,2])    
# 
library(e1071)
swiss[,2]
m <- naiveBayes(Fertility ~. , data = swiss)   
pred <- predict(m, swiss[2:6])
pred
table(pred, swiss[,2])

