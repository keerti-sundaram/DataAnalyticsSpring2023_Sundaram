pm2.5 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/pm2.5.csv", header=T)
View(pm2.5)
copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
#data imputation, nearest-neighbor imputation
library(VIM)
copd_imp <- kNN(copd[-length(copd)], k=5)
summary(copd_imp)
copd_imp <- subset(copd_imp, select=Country:X2020)
View(copd_imp)
#creating new copd data frame with years 2010-2019
temp <- subset(copd_imp, select=X2010:X2019)
copd <- data.frame(copd_imp$Country, temp)
grand_total <- rowSums(copd[-1])
copd$Grand.Total <- grand_total
colnames(copd)[1] = "Country"
#ordering to make sure comparisons are correct
pm2.5 <- pm2.5[order(pm2.5$Country),]
copd <- copd[order(copd$Country),]
View(copd)
View(pm2.5)
#adding a grand total col to PM2.5 data
grand_total <- rowSums(pm2.5[-1])
pm2.5$Grand.Total <- grand_total

#taking the subset of the data needed for the linear regression
pm2.5 <- subset(pm2.5, Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'T√ºrkiye', 'Ukraine', 'United States of America'))
copd <- subset(copd, Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'Turkey', 'Ukraine', 'United States of America'))

#year based approach
df_2010 <- data.frame(copd$Country, pm2.5$X2010, copd$X2010)
colnames(df_2010) <- c('Country', 'pm2.5', 'copd')
lm_2010 <- lm(copd ~ pm2.5, data = df_2010)
summary(lm_2010)

df_2011 <- data.frame(copd$Country, pm2.5$X2011, copd$X2011)
colnames(df_2011) <- c('Country', 'pm2.5', 'copd')
lm_2011 <- lm(copd ~ pm2.5, data = df_2011)
summary(lm_2011)

df_2012 <- data.frame(copd$Country, pm2.5$X2012, copd$X2012)
colnames(df_2012) <- c('Country', 'pm2.5', 'copd')
lm_2012 <- lm(copd ~ pm2.5, data = df_2012)
summary(lm_2012)

df_2013 <- data.frame(copd$Country, pm2.5$X2013, copd$X2013)
colnames(df_2013) <- c('Country', 'pm2.5', 'copd')
lm_2013 <- lm(copd ~ pm2.5, data = df_2013)
summary(lm_2013)

df_2014 <- data.frame(copd$Country, pm2.5$X2014, copd$X2014)
colnames(df_2014) <- c('Country', 'pm2.5', 'copd')
lm_2014 <- lm(copd ~ pm2.5, data = df_2014)
summary(lm_2014)

df_2015 <- data.frame(copd$Country, pm2.5$X2015, copd$X2015)
colnames(df_2015) <- c('Country', 'pm2.5', 'copd')
lm_2015 <- lm(copd ~ pm2.5, data = df_2015)
summary(lm_2015)

df_2016 <- data.frame(copd$Country, pm2.5$X2016, copd$X2016)
colnames(df_2016) <- c('Country', 'pm2.5', 'copd')
lm_2016 <- lm(copd ~ pm2.5, data = df_2016)
summary(lm_2016)

df_2017 <- data.frame(copd$Country, pm2.5$X2017, copd$X2017)
colnames(df_2017) <- c('Country', 'pm2.5', 'copd')
lm_2017 <- lm(copd ~ pm2.5, data = df_2017)
summary(lm_2017)

df_2018 <- data.frame(copd$Country, pm2.5$X2018, copd$X2018)
colnames(df_2018) <- c('Country', 'pm2.5', 'copd')
lm_2018 <- lm(copd ~ pm2.5, data = df_2018)
summary(lm_2018)

df_2019 <- data.frame(copd$Country, pm2.5$X2019, copd$X2019)
colnames(df_2019) <- c('Country', 'pm2.5', 'copd')
lm_2019 <- lm(copd ~ pm2.5, data = df_2019)
summary(lm_2019)

#country based approach
df <- data.frame(t(pm2.5[pm2.5$Country == 'Brazil', ]), t(copd[copd$Country == 'Brazil', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
brazil_lm <- lm(copd ~ pm2.5, data = df)
summary(brazil_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Canada', ]), t(copd[copd$Country == 'Canada', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
canada_lm <- lm(copd ~ pm2.5, data = df)
summary(canada_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Colombia', ]), t(copd[copd$Country == 'Colombia', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Colombia_lm <- lm(copd ~ pm2.5, data = df)
summary(Colombia_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'France', ]), t(copd[copd$Country == 'France', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
France_lm <- lm(copd ~ pm2.5, data = df)
summary(France_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Germany', ]), t(copd[copd$Country == 'Germany', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Germany_lm <- lm(copd ~ pm2.5, data = df)
summary(Germany_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Italy', ]), t(copd[copd$Country == 'Italy', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Italy_lm <- lm(copd ~ pm2.5, data = df)
summary(Italy_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Japan', ]), t(copd[copd$Country == 'Japan', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Japan_lm <- lm(copd ~ pm2.5, data = df)
summary(Japan_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Kazakhstan', ]), t(copd[copd$Country == 'Kazakhstan', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Kazakhstan_lm <- lm(copd ~ pm2.5, data = df)
summary(Kazakhstan_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Mexico', ]), t(copd[copd$Country == 'Mexico', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Mexico_lm <- lm(copd ~ pm2.5, data = df)
summary(Mexico_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Philippines', ]), t(copd[copd$Country == 'Philippines', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Philippines_lm <- lm(copd ~ pm2.5, data = df)
summary(Philippines_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Russian Federation', ]), t(copd[copd$Country == 'Russian Federation', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
RF_lm <- lm(copd ~ pm2.5, data = df)
summary(RF_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'South Africa', ]), t(copd[copd$Country == 'South Africa', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
S_Afr_lm <- lm(copd ~ pm2.5, data = df)
summary(S_Afr_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Spain', ]), t(copd[copd$Country == 'Spain', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Spain_lm <- lm(copd ~ pm2.5, data = df)
summary(Spain_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'T√ºrkiye', ]), t(copd[copd$Country == 'Turkey', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Turkey_lm <- lm(copd ~ pm2.5, data = df)
summary(Turkey_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'Ukraine', ]), t(copd[copd$Country == 'Ukraine', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
Ukraine_lm <- lm(copd ~ pm2.5, data = df)
summary(Ukraine_lm)

df <- data.frame(t(pm2.5[pm2.5$Country == 'United States of America', ]), t(copd[copd$Country == 'United States of America', ]))
colnames(df) <- c('pm2.5', 'copd')
df <- df[-1,] 
df$pm2.5 <- as.numeric(df$pm2.5)
df$copd <- as.numeric(df$copd)
US_lm <- lm(copd ~ pm2.5, data = df)
summary(US_lm)

#looking at ranks of the countries in terms of COPD, GDP, and PM2.5 data
copd2 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
copd_imp2 <- kNN(copd2[-length(copd2)], k=5)
copd2 <- subset(copd_imp2, select=Country:X2020)
grand_total <- rowSums(copd2[-1])
copd2$Grand.Total <- grand_total

#COPD rankings
copd2 <- copd2[order(copd2$Grand.Total, decreasing = TRUE),]
View(copd2)
rank <- 1:104
df_rank <- data.frame(copd2$Country, rank)
df_rank
rank_16 <- subset(df_rank, copd2.Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'Turkey', 'Ukraine', 'United States of America'))
rank_16

gdp2 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/gdp_csv.csv", header=T)
gdp_imp2 <- kNN(gdp2[-length(gdp2)], k=5)
gdp2 <- subset(gdp_imp2, select=Country:X2020)
grand_total <- rowSums(gdp2[-1])
gdp2$Grand.Total <- grand_total

#GDP rankings
gdp2 <- gdp2[order(gdp2$Grand.Total, decreasing = TRUE),]
View(gdp2)
df_rank_gdp <- data.frame(gdp2$Country, rank)
df_rank_gdp
rank_16_gdp <- subset(df_rank_gdp, gdp2.Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'Turkey', 'Ukraine', 'United States of America'))
rank_16_gdp

#PM2.5 rankings
pm2.5_2 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/pm2.5.csv", header=T)
grand_total <- rowSums(pm2.5_2[-1])
pm2.5_2$Grand.Total <- grand_total
pm2.5_2 <- pm2.5_2[order(pm2.5_2$Grand.Total, decreasing = TRUE),]
View(pm2.5_2)
df_rank_pm <- data.frame(pm2.5_2$Country, rank)
df_rank_pm
rank_16_pm <- subset(df_rank_pm, pm2.5_2.Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'T√ºrkiye', 'Ukraine', 'United States of America'))
rank_16_pm
