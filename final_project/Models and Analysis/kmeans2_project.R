gdp <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/gdp_csv.csv", header=T)
#knn imputation
library(VIM)
gdp_imp <- kNN(gdp[-length(gdp)], k=5)
gdp_imp <- subset(gdp_imp, select=Country:X2020)
grand_total <- rowSums(gdp_imp[-1])
gdp_imp$Grand.Total <- grand_total

library(ISLR)
library(cluster)
library(factoextra)
set.seed(101)

#Grand total clustering
totalClusters <- kmeans(gdp_imp$Grand.Total, 4, nstart = 20) # nstart is the number of random start
print(totalClusters)
table(totalClusters$cluster)
clusplot(gdp_imp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0)

#find optimal number of clusters
fviz_nbclust(gdp_imp[-1], kmeans, method="wss") #bend at 4
totalClusters <- kmeans(gdp_imp[-1], 4, nstart = 20) # nstart is the number of random start
print(totalClusters$cluster)
clusplot(gdp_imp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Percent of GDP Spent on Health Care 2000-2020")
df <- data.frame(gdp_imp$Country, totalClusters$cluster)
df

#attempting the similar group-split as the COPD data, did not end up using in the analysis 

#split into groups
group1<- data.frame(gdp_imp$Country,gdp_imp[,c(2:6)])
group2<- data.frame(gdp_imp$Country, gdp_imp[,c(7:11)])
group3<- data.frame(gdp_imp$Country, gdp_imp[,c(12:16)])
group4<- data.frame(gdp_imp$Country, gdp_imp[,c(17:22)])
group4
fviz_nbclust(group4[-1], kmeans, method="wss") #bend at 4

#group 1
group1Clusters <- kmeans(group1[-1], 4, nstart = 20) 
clusplot(group1,group1Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Percent of GDP Spent on Health Care 2000-2004")
group1_clusters <- data.frame(gdp_imp$Country, group1Clusters$cluster)
group1_clusters

#group 2
group2Clusters <- kmeans(group2[-1], 4, nstart = 20) 
clusplot(group2,group2Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0,main = "Percent of GDP Spent on Health Care 2005-2009")
group2_clusters <- data.frame(gdp_imp$Country, group2Clusters$cluster)

#group 3
group3Clusters <- kmeans(group3[-1], 4, nstart = 20) 
clusplot(group3,group3Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Percent of GDP Spent on Health Care 2010-2014")
group3_clusters <- data.frame(gdp_imp$Country, group3Clusters$cluster)

#group 4
group4Clusters <- kmeans(group4[-1], 4, nstart = 20) 
clusplot(group4,group4Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Percent of GDP Spent on Health Care 2015-2020")
group4_clusters <- data.frame(gdp_imp$Country, group4Clusters$cluster)

#renaming columns for simplicity
colnames(df) <- c("Country", "Cluster")
colnames(group1_clusters) <- c("Country", "Cluster")
colnames(group2_clusters) <- c("Country", "Cluster")
colnames(group3_clusters) <- c("Country", "Cluster")
colnames(group4_clusters) <- c("Country", "Cluster")

#Grand total
Countries_GT_1 <- (df[df$Cluster == 1,])$Country
Countries_GT_1
Countries_GT_2 <- (df[df$Cluster == 2,])$Country
Countries_GT_3  <- (df[df$Cluster == 3,])$Country
Countries_GT_4 <-  (df[df$Cluster == 4,])$Country

#group 1
Countries_G1_1 <-  (group1_clusters[group1_clusters$Cluster == 1,])$Country
Countries_G1_2 <-  (group1_clusters[group1_clusters$Cluster == 2,])$Country
Countries_G1_3 <-  (group1_clusters[group1_clusters$Cluster == 3,])$Country
Countries_G1_4 <-  (group1_clusters[group1_clusters$Cluster == 4,])$Country

#group 2
Countries_G2_1 <-  (group2_clusters[group2_clusters$Cluster == 1,])$Country
Countries_G2_2 <-  (group2_clusters[group2_clusters$Cluster == 2,])$Country
Countries_G2_3 <-  (group2_clusters[group2_clusters$Cluster == 3,])$Country
Countries_G2_4 <- (group2_clusters[group2_clusters$Cluster == 4,])$Country

#group 3
Countries_G3_1 <-  (group3_clusters[group3_clusters$Cluster == 1,])$Country
Countries_G3_2 <-  (group3_clusters[group3_clusters$Cluster == 2,])$Country
Countries_G3_3 <-  (group3_clusters[group3_clusters$Cluster == 3,])$Country
Countries_G3_4 <-  (group3_clusters[group3_clusters$Cluster == 4,])$Country

#group 4
Countries_G4_1 <-  (group4_clusters[group4_clusters$Cluster == 1,])$Country
Countries_G4_2 <-  (group4_clusters[group4_clusters$Cluster == 2,])$Country
Countries_G4_3 <-  (group4_clusters[group4_clusters$Cluster == 3,])$Country
Countries_G4_4 <-  (group4_clusters[group4_clusters$Cluster == 4,])$Country

cat(Countries_GT_1, sep="\n")
cat(Countries_GT_2, sep="\n")
cat(Countries_GT_3, sep="\n")
cat(Countries_GT_4, sep="\n")

#finding most similar clusters
setdiff(Countries_GT_1, Countries_G1_1)
setdiff(Countries_GT_2, Countries_G1_2)
setdiff(Countries_GT_3, Countries_G1_3)
setdiff(Countries_GT_4, Countries_G1_4)

setdiff(Countries_G1_1, Countries_G2_4)
setdiff(Countries_G1_2, Countries_G2_2)
setdiff(Countries_G1_3, Countries_G2_1)
setdiff(Countries_G1_4, Countries_G2_3)

setdiff(Countries_G2_1, Countries_G3_2)
setdiff(Countries_G2_2, Countries_G3_1)
setdiff(Countries_G2_3, Countries_G3_4)
setdiff(Countries_G2_4, Countries_G3_3)

setdiff(Countries_G3_1, Countries_G4_2)
setdiff(Countries_G3_2, Countries_G4_3)
setdiff(Countries_G3_3, Countries_G4_1)
setdiff(Countries_G3_4, Countries_G4_4)

setdiff(Countries_GT_1, Countries_G4_1)
setdiff(Countries_GT_2, Countries_G4_2)
setdiff(Countries_GT_3, Countries_G4_3)
setdiff(Countries_GT_4, Countries_G4_4)

#getting similar clusters (countries that didn't switch among the years)
clus1 <- (intersect((intersect((intersect(Countries_G1_1, Countries_G2_4)), Countries_G3_3)), Countries_G4_1))
clus1
clus2 <- (intersect((intersect((intersect(Countries_G1_2, Countries_G2_2)), Countries_G3_1)), Countries_G4_2))
clus2
clus3 <- (intersect((intersect((intersect(Countries_G1_3, Countries_G2_1)), Countries_G3_2)), Countries_G4_3))
clus3
clus4 <- (intersect((intersect((intersect(Countries_G1_4, Countries_G2_3)), Countries_G3_4)), Countries_G4_4))
clus4

cat(clus1, sep="\n")
cat(clus2, sep="\n")
cat(clus3, sep="\n")
cat(clus4, sep="\n")

#finding countries that switched
countries_clustered <- c(clus1, clus2, clus3, clus4)
total_countries <- gdp_imp$Country
#42 countries "switched"
cat((setdiff(total_countries, countries_clustered)), sep="\n")
