copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
#data imputation, nearest-neighbor imputation
library(VIM)
copd_imp <- kNN(copd[-length(copd)], k=5)
summary(copd_imp)
copd_imp <- subset(copd_imp, select=Country:X2020)
grand_total <- rowSums(copd_imp[-1])
copd_imp$Grand.Total <- grand_total
copd <- copd_imp

population <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/population.csv", header=T)
View(population)
copd <- copd[order(copd$Country),]
population <- population[order(population$Country),]
copd_no_labels <- copd_imp[-1]
ratios <- cbind(population[1], round(copd_no_labels[-length(copd_no_labels)]/population[-1],5))
View(ratios)

library(ISLR)
library(cluster)
#find optimal number of clusters
library(factoextra)
fviz_nbclust(ratios[-1], kmeans, method="wss") #bend at 4
totalClusters <- kmeans(ratios[-1], 4, nstart = 20) # nstart is the number of random start
print(totalClusters$cluster)
clusplot(ratios,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0,  main = "Number of Deaths Due to COPD from 2000-2020")
df <- data.frame(copd$Country, totalClusters$cluster)
df
