copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
gdp <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/gdp_csv.csv", header=T)
View(copd)
View(gdp)
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
View(demo_imp)


copd_Albania <- copd[copd$Country == "Albania",]
names(copd_Albania) <- NULL

#removing country label and grand total
c_Albania <- copd_Albania[-1]
c_Albania <- c_Albania[-length(c_Albania)]
hist(as.numeric(c_Albania))
c_Albania
gdp_Albania <- gdp[gdp$Country == "Albania",]
gdp_Albania
g_Albania <- gdp_Albania[!is.na(gdp_Albania)]
g_Albania        
#removing country label and grand total
g_Albania <- g_Albania[-1]
g_Albania <- g_Albania[-length(g_Albania)]
hist(as.numeric(g_Albania))
years <- c("X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019","X2020")

c_transpose = t(c_Albania)
df <- data.frame(years, g_Albania, c_transpose)
df


tf <- is.na(copd$X2001)
c_2001 <- copd$X2001[!tf]
mean(c_2001)
median(c_2001)
hist(c_2001)

tf <- is.na(copd$X2019)
c_2019 <- copd$X2019[!tf]
mean(c_2019)
median(c_2019)
hist(c_2019)
boxplot(c_2001, c_2019)


tf <- is.na(copd$X2005)
c_2001 <- copd$X2005[!tf]
mean(c_2001)
median(c_2001)
hist(c_2001)

tf <- is.na(copd$X2015)
c_2019 <- copd$X2015[!tf]
mean(c_2019)
median(c_2019)
hist(c_2019)
boxplot(c_2001, c_2019)


tf <- is.na(copd$X2009)
c_2001 <- copd$X2009[!tf]
mean(c_2001)
median(c_2001)
hist(c_2001)

tf <- is.na(copd$X2014)
c_2019 <- copd$X2014[!tf]
mean(c_2019)
median(c_2019)
hist(c_2019)
boxplot(c_2001, c_2019)


copd_us <- copd[copd$Country == "United States of America",]
c_us <- copd_us[!is.na(copd_us)]
c_us <- c_us[-1]
c_us <- c_us[-length(c_us)]
plot(as.numeric(c_us))

tf <- is.na(gdp$X2001)
g_2001<- gdp$X2001[!tf]
mean(g_2001)
median(g_2001)

tf <- is.na(gdp$X2020)
g_2020<- gdp$X2020[!tf]
mean(g_2020)
median(g_2020)
boxplot(g_2001, g_2020)

tf <- is.na(gdp$X2005)
g_2001<- gdp$X2005[!tf]
mean(g_2001)
median(g_2001)

tf <- is.na(gdp$X2015)
g_2020<- gdp$X2015[!tf]
mean(g_2020)
median(g_2020)
boxplot(g_2001, g_2020)

tf <- is.na(gdp$X2008)
g_2001<- gdp$X2008[!tf]
mean(g_2001)
median(g_2001)

tf <- is.na(gdp$X2017)
g_2020<- gdp$X2017[!tf]
mean(g_2020)
median(g_2020)
boxplot(g_2001, g_2020)


copd_China <- copd[copd$Country == "China, Hong Kong SAR",]
copd_China

gdp_China<- gdp[gdp$Country == "China",]
gdp_China
tf <- is.na(gdp_China)
g_China <- gdp_China[!tf]
g_China <- g_China[-length(g_China)]
mean(as.numeric(g_China[-1]))

copd_Kuwait<- copd[copd$Country == "Kuwait",]
copd_Kuwait

gdp_Kuwait<- gdp[gdp$Country == "Kuwait",]
gdp_Kuwait
tf <- is.na(gdp_Kuwait)
g_Kuwait <- gdp_Kuwait[!tf]
g_Kuwait <- g_Kuwait[-length(g_Kuwait)]
mean(as.numeric(g_Kuwait[-1]))

copd_Tajikistan<- copd[copd$Country == "Tajikistan",]
copd_Tajikistan

gdp_Tajikistan<- gdp[gdp$Country == "Tajikistan",]
gdp_Tajikistan
tf <- is.na(gdp_Tajikistan)
g_Tajikistan <- gdp_Tajikistan[!tf]
g_Tajikistan <- g_Tajikistan[-length(g_Tajikistan)]
mean(as.numeric(g_Tajikistan[-1]))

copd_Egypt<- copd[copd$Country == "Egypt",]
copd_Egypt

gdp_Egypt<- gdp[gdp$Country == "Egypt",]
gdp_Egypt
tf <- is.na(gdp_Egypt)
g_Egypt <- gdp_Egypt[!tf]
g_Egypt <- g_Egypt[-length(g_Egypt)]
mean(as.numeric(g_Egypt[-1]))

copd_Bahrain<- copd[copd$Country == "Bahrain",]
copd_Bahrain

gdp_Bahrain<- gdp[gdp$Country == "Bahrain",]
gdp_Bahrain
tf <- is.na(gdp_Bahrain)
g_Bahrain <- gdp_Bahrain[!tf]
g_Bahrain <- g_Bahrain[-length(g_Bahrain)]
mean(as.numeric(g_Bahrain[-1]))

gdp_Bahamas<- gdp[gdp$Country == "Bahamas",]
gdp_Bahamas
tf <- is.na(gdp_Bahamas)
g_Bahamas <- gdp_Bahamas[!tf]
g_Bahamas <- g_Bahamas[-length(g_Bahamas)]
mean(as.numeric(g_Bahamas[-1]))

gdp_Bahamas<- gdp[gdp$Country == "Finland",]
gdp_Bahamas
tf <- is.na(gdp_Bahamas)
g_Bahamas <- gdp_Bahamas[!tf]
g_Bahamas <- g_Bahamas[-length(g_Bahamas)]
mean(as.numeric(g_Bahamas[-1]))

gdp_Bahamas<- gdp[gdp$Country == "Iceland",]
gdp_Bahamas
tf <- is.na(gdp_Bahamas)
g_Bahamas <- gdp_Bahamas[!tf]
g_Bahamas <- g_Bahamas[-length(g_Bahamas)]
mean(as.numeric(g_Bahamas[-1]))

gdp_Bahamas<- gdp[gdp$Country == "Sweden",]
gdp_Bahamas
tf <- is.na(gdp_Bahamas)
g_Bahamas <- gdp_Bahamas[!tf]
g_Bahamas <- g_Bahamas[-length(g_Bahamas)]
mean(as.numeric(g_Bahamas[-1]))

gdp_Bahamas<- gdp[gdp$Country == "Norway",]
gdp_Bahamas
tf <- is.na(gdp_Bahamas)
g_Bahamas <- gdp_Bahamas[!tf]
g_Bahamas <- g_Bahamas[-length(g_Bahamas)]
mean(as.numeric(g_Bahamas[-1]))

tf <- is.na(gdp$X2010)
g_2010 <- gdp$X2010[!tf]
sclass <- kmeans(g_2010, 3) 
help(kmeans)
table(sclass$cluster, g_2010)    
plot(g_2010)
text(g_2010, labels = gdp$Country)
