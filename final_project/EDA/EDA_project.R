#investigating the data and looking at the structure
#read in data
copd <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/mortality_csv.csv", header=T)
gdp <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/gdp_csv.csv", header=T)
View(copd)
View(gdp)
str(copd)
library(caret)
library(RANN)
#data imputation (technique using caret library, not used later)
imp <- preProcess(copd, method = "medianImpute")
copd_imp <- predict(imp, copd)
View(copd_imp)
#need to update grand totals
no_labels <- copd_imp[-1]
grand_total <- rowSums(no_labels[-length(no_labels)])
copd_imp$Grand.Total <- grand_total
View(demo_imp)

#filtering data by country
copd_Albania <- copd[copd$Country == "Albania",]
names(copd_Albania) <- NULL

#removing country label and grand total
c_Albania <- copd_Albania[-1]
c_Albania <- c_Albania[-length(c_Albania)]
#histogram of individual country
hist(as.numeric(c_Albania))
c_Albania
#gdp one country filtering
gdp_Albania <- gdp[gdp$Country == "Albania",]
gdp_Albania
g_Albania <- gdp_Albania[!is.na(gdp_Albania)]
g_Albania        
#removing country label and grand total
g_Albania <- g_Albania[-1]
g_Albania <- g_Albania[-length(g_Albania)]
hist(as.numeric(g_Albania))

#looking at a specific year
tf <- is.na(copd$X2001)
c_2001 <- copd$X2001[!tf]
mean(c_2001)
median(c_2001)
hist(c_2001)
#looking at US data
copd_us <- copd[copd$Country == "United States of America",]
c_us <- copd_us[!is.na(copd_us)]
c_us <- c_us[-1]
c_us <- c_us[-length(c_us)]
plot(as.numeric(c_us))
#looking at GDP for a specific year
tf <- is.na(gdp$X2001)
g_2001<- gdp$X2001[!tf]
mean(g_2001)
median(g_2001)

#used for assignment 5 presentation, looking at COPD deaths and percent spent on healthcare
#for countries with top 5 highest PM2.5 mean exposure and lowest 5 PM2.5 mean exposure
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

gdp_Finland<- gdp[gdp$Country == "Finland",]
gdp_Finland
tf <- is.na(gdp_Finland)
g_Finland <- gdp_Finland[!tf]
g_Finland <- g_Finland[-length(g_Finlands)]
mean(as.numeric(g_Finland[-1]))

gdp_Iceland<- gdp[gdp$Country == "Iceland",]
gdp_Iceland
tf <- is.na(gdp_Iceland)
g_Iceland <- gdp_Iceland[!tf]
g_Iceland <- g_Iceland[-length(g_Iceland)]
mean(as.numeric(g_Iceland[-1]))

gdp_Sweden<- gdp[gdp$Country == "Sweden",]
gdp_Sweden
tf <- is.na(gdp_Sweden)
g_Sweden <- gdp_Sweden[!tf]
g_Sweden <- g_Sweden[-length(g_Sweden)]
mean(as.numeric(g_Sweden[-1]))

gdp_Norway<- gdp[gdp$Country == "Norway",]
gdp_Norway
tf <- is.na(gdp_Norway)
g_Norway <- gdp_Norway[!tf]
g_Norway <- g_Norway[-length(g_Norway)]
mean(as.numeric(g_Norway[-1]))
