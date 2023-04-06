copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
library(VIM)
help(kNN)
copd_imp1 <- kNN(copd[-length(copd)], k=3)
View(copd_imp1)
summary(copd_imp1)

copd_imp1 <- subset(copd_imp1, select=Country:X2020)
View(copd_imp1)
grand_total <- rowSums(copd_imp1[-1])
copd_imp1$Grand.Total <- grand_total
