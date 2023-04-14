copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
View(copd)
str(copd)
library(caret)
library(RANN)
#data imputation
imp <- preProcess(copd, method = "medianImpute")
copd_imp <- predict(imp, copd)
View(copd_imp)
#need to update grand totals
no_labels <- copd_imp[-1]
grand_total <- rowSums(no_labels[-length(no_labels)])
copd_imp$Grand.Total <- grand_total
View(copd_imp)

library(ISLR)
set.seed(101)


totalClusters <- kmeans(copd_imp$Grand.Total, 4, nstart = 20) # nstart is the number of random start
print(totalClusters)
table(totalClusters$cluster)
library(cluster)
clusplot(copd_imp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)



#find optimal number of clusters
library(factoextra)
fviz_nbclust(copd_imp[-1], kmeans, method="wss") #bend at 4
totalClusters <- kmeans(copd_imp[-1], 4, nstart = 20) # nstart is the number of random start
print(totalClusters$cluster)
#new data frame matching country to cluster 
df <- data.frame(copd_imp$Country, totalClusters$cluster)
df
clusplot(copd_imp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)

group1<- data.frame(copd_imp$Country, copd_imp[,c(2:6)])
group2<- data.frame(copd_imp$Country, copd_imp[,c(7:11)])
group3<- data.frame(copd_imp$Country, copd_imp[,c(12:16)])
group4<- data.frame(copd_imp$Country, copd_imp[,c(17:22)])

group1Clusters <- kmeans(group1[-1], 4, nstart = 20) 
clusplot(group1,group1Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)
group1_clusters <- data.frame(copd_imp$Country, group1Clusters$cluster)
group1_clusters

group2Clusters <- kmeans(group2[-1], 4, nstart = 20) 
clusplot(group2,group2Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)
group2_clusters <- data.frame(copd_imp$Country, group2Clusters$cluster)

group3Clusters <- kmeans(group3[-1], 4, nstart = 20) 
clusplot(group3,group3Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)
group3_clusters <- data.frame(copd_imp$Country, group3Clusters$cluster)

group4Clusters <- kmeans(group4[-1], 4, nstart = 20) 
clusplot(group4,group4Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)
group4_clusters <- data.frame(copd_imp$Country, group4Clusters$cluster)
