copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
#data imputation, nearest-neighbor imputation
library(VIM)
copd_imp <- kNN(copd[-length(copd)], k=5)
summary(copd_imp)
copd_imp <- subset(copd_imp, select=Country:X2020)
grand_total <- rowSums(copd_imp[-1])
copd_imp$Grand.Total <- grand_total
View(copd_imp)

library(ISLR)
set.seed(101)
library(cluster)
#find optimal number of clusters
library(factoextra)
fviz_nbclust(copd_imp[-1], kmeans, method="wss") #bend at 4
totalClusters <- kmeans(copd_imp[-1], 4, nstart = 20) # nstart is the number of random start
print(totalClusters$cluster)
#new data frame matching country to cluster 
df <- data.frame(copd_imp$Country, totalClusters$cluster)
df
clusplot(copd_imp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0,  main = "Number of Deaths Due to COPD from 2000-2020")

#splitting up the data into 4 groups
group1<- data.frame(copd_imp$Country, copd_imp[,c(2:6)])
group2<- data.frame(copd_imp$Country, copd_imp[,c(7:11)])
group3<- data.frame(copd_imp$Country, copd_imp[,c(12:16)])
group4<- data.frame(copd_imp$Country, copd_imp[,c(17:22)])

#group 1: 2000-2004
group1Clusters <- kmeans(group1[-1], 4, nstart = 20) 
clusplot(group1,group1Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Number of Deaths Due to COPD from 2000-2004")
group1_clusters <- data.frame(copd_imp$Country, group1Clusters$cluster)
group1_clusters

#group 2: 2005-2009
group2Clusters <- kmeans(group2[-1], 4, nstart = 20) 
clusplot(group2,group2Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Number of Deaths Due to COPD from 2005-2009")
group2_clusters <- data.frame(copd_imp$Country, group2Clusters$cluster)

#group 3: 2010-2014
group3Clusters <- kmeans(group3[-1], 4, nstart = 20) 
clusplot(group3,group3Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Number of Deaths Due to COPD from 2010-2014")
group3_clusters <- data.frame(copd_imp$Country, group3Clusters$cluster)

#group 4: 2015-2020
group4Clusters <- kmeans(group4[-1], 4, nstart = 20) 
clusplot(group4,group4Clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Number of Deaths Due to COPD from 2015-2020")
group4_clusters <- data.frame(copd_imp$Country, group4Clusters$cluster)

#labeling data frames for each group with country and it's corresponding cluster
colnames(df) <- c("Country", "Cluster")
colnames(group1_clusters) <- c("Country", "Cluster")
colnames(group2_clusters) <- c("Country", "Cluster")
colnames(group3_clusters) <- c("Country", "Cluster")
colnames(group4_clusters) <- c("Country", "Cluster")

#grand total clustering 
Countries_GT_1 <- (df[df$Cluster == 1,])$Country
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

#listing countries in each grand total cluster
cat(Countries_GT_1, sep="\n")
cat(Countries_GT_2, sep="\n")
cat(Countries_GT_3, sep="\n")
cat(Countries_GT_4, sep="\n")

#finding similarities/differences between each group
setdiff(Countries_GT_1, Countries_G1_3)
setdiff(Countries_GT_2, Countries_G1_4)
setdiff(Countries_GT_3, Countries_G1_1)
setdiff(Countries_GT_4, Countries_G1_2)

setdiff(Countries_G1_1, Countries_G2_2)
setdiff(Countries_G1_2, Countries_G2_3)
setdiff(Countries_G1_3, Countries_G2_4)
setdiff(Countries_G1_4, Countries_G2_1)

setdiff(Countries_G2_1, Countries_G3_2)
setdiff(Countries_G2_2, Countries_G3_4)
setdiff(Countries_G2_3, Countries_G3_1)
setdiff(Countries_G2_4, Countries_G3_3)

setdiff(Countries_G3_1, Countries_G4_2)
setdiff(Countries_G3_2, Countries_G4_1)
setdiff(Countries_G3_3, Countries_G4_3)
setdiff(Countries_G3_4, Countries_G4_4)

#finding the most similar clusters (i.e. countries that did not "switch" clusters)
clus1 <- (intersect((intersect((intersect(Countries_G1_1, Countries_G2_2)), Countries_G3_4)), Countries_G4_4))

clus2 <- (intersect((intersect((intersect(Countries_G1_2, Countries_G2_3)), Countries_G3_1)), Countries_G4_2))

clus3 <- (intersect((intersect((intersect(Countries_G1_3, Countries_G2_4)), Countries_G3_3)), Countries_G4_3))

clus4 <- (intersect((intersect((intersect(Countries_G1_4, Countries_G2_1)), Countries_G3_2)), Countries_G4_1))


cat(clus1, sep="\n")
cat(clus2, sep="\n")
cat(clus3, sep="\n")
cat(clus4, sep="\n")

#finding which countries switched
countries_clustered <- c(clus1, clus2, clus3, clus4)
total_countries <- copd_imp$Country
#14 countries "switched"
cat((setdiff(total_countries, countries_clustered)), sep="\n")

