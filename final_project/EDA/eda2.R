copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
str(copd)
summary(copd)
#find number of NAs
sum(is.na(copd))
#filtering out country labels and grand totals for boxplots
no_labels_copd <- copd[-1]
boxplot(no_labels_copd[-length(no_labels_copd)], main = "COPD cases of all Countries over 2000-2020")
hist(copd$Grand.Total, main = "Grand Total Deaths due COPD")

gdp <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/gdp_csv.csv", header=T)
str(gdp)
summary(gdp)
#find number of NAs
sum(is.na(gdp))
no_labels_gdp <- gdp[-1]
boxplot(no_labels_gdp[-length(no_labels_gdp)], main = "Percent of GDP Spent on Health of all Countries over 2000-2020")
hist(gdp$Grand.Total, main = "Grand Total Percent of GDP Spent on Health")

pm2.5 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/pm2.5.csv", header=T)
str(pm2.5)
summary(pm2.5)
#adding a grand total column to maintain same structure as copd and gdp datasets
grand_total <- rowSums(pm2.5[-1])
pm2.5$Grand.Total <- grand_total
#find number of NAs
sum(is.na(pm2.5))
no_labels_pm2.5 <- pm2.5[-1]
boxplot(no_labels_pm2.5[-length(no_labels_pm2.5)], main = "PM2.5 Concentrations of all Countries over 2010-2019")
hist(pm2.5$Grand.Total, main = "Grand Total PM2.5 Concentrations")

#to analyze distributions
plot(density(copd_imp$Grand.Total), main="Density Plot of COPD Grand Total")
plot(density(gdp_imp$Grand.Total), main= "Density Plot of GDP Grand Total")
plot(density(pm2.5$Grand.Total), main= "Density Plot of PM2.5 Grand Total")

