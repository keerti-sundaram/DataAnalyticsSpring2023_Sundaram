pm2.5 <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/Final_Project/pm2.5.csv", header=T)
View(pm2.5)
copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
#data imputation, nearest-neighbor imputation
library(VIM)
copd_imp <- kNN(copd[-length(copd)], k=5)
summary(copd_imp)
copd_imp <- subset(copd_imp, select=Country:X2020)
#creating new copd data frame with years 2010-2019
temp <- subset(copd_imp, select=X2010:X2019)
copd <- data.frame(copd_imp$Country, temp)
grand_total <- rowSums(copd[-1])
copd$Grand.Total <- grand_total
colnames(copd)[1] = "Country"
pm2.5 <- pm2.5[order(pm2.5$Country),]
copd <- copd[order(copd$Country),]
View(copd)
View(pm2.5)
grand_total <- rowSums(pm2.5[-1])
pm2.5$Grand.Total <- grand_total

pm2.5 <- subset(pm2.5, Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'T√ºrkiye', 'Ukraine', 'United States of America'))
copd <- subset(copd, Country %in% c('Brazil', 'Canada', 'Colombia', 'France', 'Germany', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'Russian Federation', 'South Africa', 'Spain', 'Turkey', 'Ukraine', 'United States of America'))

df_all <- data.frame(copd$Country, pm2.5$Grand.Total, copd$Grand.Total)
colnames(df_all) <- c('Country', 'pm2.5', 'copd')
lm_all <- lm(copd ~ pm2.5, data = df_all)
summary(lm_all)
plot(df_all$pm2.5, df_all$copd)
abline(lm_all)

df_2010 <- data.frame(copd$Country, pm2.5$X2010, copd$X2010)
colnames(df_2010) <- c('Country', 'pm2.5', 'copd')
lm_2010 <- lm(copd ~ pm2.5, data = df_2010)
summary(lm_2010)
plot(df_2010$pm2.5, df_2010$copd)
abline(lm_2010)

df_2011 <- data.frame(copd$Country, pm2.5$X2011, copd$X2011)
colnames(df_2011) <- c('Country', 'pm2.5', 'copd')
lm_2011 <- lm(copd ~ pm2.5, data = df_2011)
summary(lm_2011)
plot(df_2011$pm2.5, df_2011$copd)
abline(lm_2011)

df_2012 <- data.frame(copd$Country, pm2.5$X2012, copd$X2012)
colnames(df_2012) <- c('Country', 'pm2.5', 'copd')
lm_2012 <- lm(copd ~ pm2.5, data = df_2012)
summary(lm_2012)
plot(df_2012$pm2.5, df_2012$copd)
abline(lm_2012)

df_2013 <- data.frame(copd$Country, pm2.5$X2013, copd$X2013)
colnames(df_2013) <- c('Country', 'pm2.5', 'copd')
lm_2013 <- lm(copd ~ pm2.5, data = df_2013)
summary(lm_2013)
plot(df_2013$pm2.5, df_2013$copd)
abline(lm_2013)

df_2014 <- data.frame(copd$Country, pm2.5$X2014, copd$X2014)
colnames(df_2014) <- c('Country', 'pm2.5', 'copd')
lm_2014 <- lm(copd ~ pm2.5, data = df_2014)
summary(lm_2014)
plot(df_2014$pm2.5, df_2014$copd)
abline(lm_2014)

df_2015 <- data.frame(copd$Country, pm2.5$X2015, copd$X2015)
colnames(df_2015) <- c('Country', 'pm2.5', 'copd')
lm_2015 <- lm(copd ~ pm2.5, data = df_2015)
summary(lm_2015)
plot(df_2015$pm2.5, df_2015$copd)
abline(lm_2015)

df_2016 <- data.frame(copd$Country, pm2.5$X2016, copd$X2016)
colnames(df_2016) <- c('Country', 'pm2.5', 'copd')
lm_2016 <- lm(copd ~ pm2.5, data = df_2016)
summary(lm_2016)
plot(df_2016$pm2.5, df_2016$copd)
abline(lm_2016)

df_2017 <- data.frame(copd$Country, pm2.5$X2017, copd$X2017)
colnames(df_2017) <- c('Country', 'pm2.5', 'copd')
lm_2017 <- lm(copd ~ pm2.5, data = df_2017)
summary(lm_2017)
plot(df_2017$pm2.5, df_2017$copd)
abline(lm_2017)

df_2018 <- data.frame(copd$Country, pm2.5$X2018, copd$X2018)
colnames(df_2018) <- c('Country', 'pm2.5', 'copd')
lm_2018 <- lm(copd ~ pm2.5, data = df_2018)
summary(lm_2018)
plot(df_2018$pm2.5, df_2018$copd)
abline(lm_2018)

df_2019 <- data.frame(copd$Country, pm2.5$X2019, copd$X2019)
colnames(df_2019) <- c('Country', 'pm2.5', 'copd')
lm_2019 <- lm(copd ~ pm2.5, data = df_2019)
summary(lm_2019)
plot(df_2019$pm2.5, df_2019$copd)
abline(lm_2019)


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
plot(df$pm2.5, df$copd)
abline(France_lm)

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

