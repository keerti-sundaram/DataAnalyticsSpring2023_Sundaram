copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
#data imputation, nearest-neighbor imputation
library(VIM)
copd_imp <- kNN(copd[-length(copd)], k=5)
summary(copd_imp)
copd_imp <- subset(copd_imp, select=Country:X2020)
grand_total <- rowSums(copd_imp[-1])
copd_imp$Grand.Total <- grand_total

population <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/population.csv", header=T)
View(population)
View(copd_imp)
copd_imp <- copd_imp[order(copd_imp$Country),]
population <- population[order(population$Country),]
population[-1]/copd_imp[-1]
save <- copd_imp[-1]
temp <- population[-1]/save[-length(save)]
temp <- cbind(population[1], round(save[-length(save)]/population[-1],5))
View(temp)
summary(temp)
library(ISLR)
set.seed(101)
library(cluster)
#find optimal number of clusters
library(factoextra)
fviz_nbclust(temp[-1], kmeans, method="wss") #bend at 4
totalClusters <- kmeans(temp[-1], 4, nstart = 20) # nstart is the number of random start
print(totalClusters$cluster)
clusplot(temp,totalClusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0,  main = "Number of Deaths Due to COPD from 2000-2020")

df2010 <- data.frame(temp$Country, temp$X2010, pm2.5$X2010)
View(df2010)
lm2010 <- lm(df2010$temp.X2010 ~ df2010$pm2.5.X2010)
summary(lm2010)
