###KEERTI SUNDARAM
###DATA ANALYTICS: LAB 1

#Reading in excel file
#library("xlsx")
#EPI_data <- read.xlsx("/Users/keertisundaram/Dropbox/Data Analytics/lab1/2010EPI_data.xls", 4)

#Reading in csv
EPI_data <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/lab1/EPI2010_data.csv", header=T, skip = 1)

#Data exploration
EPI_data
View(EPI_data)
attach(EPI_data)

#N/A filtering
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
View(EPI_data)

#-----------EXERCISE 1 -----------

#Data exploration
summary(EPI)
fivenum(EPI_data$EPI, na.rm=TRUE)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_data$EPI, na.rm=TRUE, bw=1.))
rug(EPI)

#Fitting a distribution
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI)
qqline(EPI)

x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#Exploration and Fitting with DALY
summary(DALY)
fivenum(DALY, na.rm=TRUE)
stem(DALY)
hist(DALY)
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(DALY)
qqline(DALY)

#Exploration and Fitting with WATER_H
summary(WATER_H)
fivenum(WATER_H, na.rm=TRUE)
stem(WATER_H)
hist(WATER_H)
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(WATER_H)
qqline(WATER_H)

#comparing distributions
boxplot(EPI, ENVHEALTH, main = "EPI and ENVHEALTH")
boxplot(EPI, ECOSYSTEM,  main = "EPI and ECOSYSTEM")
boxplot(EPI, DALY,  main = "EPI and DALY")
boxplot(EPI, AIR_H,  main = "EPI and AIR_H")
boxplot(EPI, WATER_H,  main = "EPI and WATER_H")
boxplot(EPI, AIR_E,  main = "EPI and AIR_E")
boxplot(EPI, WATER_E,  main = "EPI and WATER_E")
boxplot(EPI, BIODIVERSITY,  main = "EPI and BIODIVERSITY")

#-----------EXERCISE 2 -----------

#Conditional Filtering
EPILand<-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

#repeat exercise 1 
summary(ELand)
fivenum(ELand, na.rm = TRUE)
stem(ELand)
plot(ecdf(ELand), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(ELand)

#Surface Water
EPISurfaceWater<-EPI[!No_surface_water]
EWater <- EPISurfaceWater[!is.na(EPISurfaceWater)]
hist(EWater)
hist(EWater, seq(30., 95., 1.0), prob=TRUE)

summary(EWater)
fivenum(EWater, na.rm = TRUE)
stem(EWater)
plot(ecdf(EWater), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EWater)

#Desert
EPINo_Desert<-EPI[!Desert]
ENDesert <- EPINo_Desert[!is.na(EPINo_Desert)]
hist(ENDesert)
hist(ENDesert, seq(30., 95., 1.0), prob=TRUE)

summary(ENDesert)
fivenum(ENDesert, na.rm = TRUE)
stem(ENDesert)
plot(ecdf(ENDesert), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(ENDesert)

#High Population Density
EPINo_Pop<-EPI[!High_Population_Density]
ENPop <- EPINo_Pop[!is.na(EPINo_Pop)]
hist(ENPop)
hist(ENPop, seq(30., 95., 1.0), prob=TRUE)

#Filtering on EPI regions
EPI_South_Asia <- EPI[EPI_regions == "South Asia"]
EPI_S_Asia <- EPI_South_Asia[!is.na(EPI_South_Asia)]
hist(EPI_S_Asia)
hist(EPI_S_Asia, seq(30., 95., 1.0), prob=TRUE)


#GPW3_GRUMP

#Reading in the data
GPW3_data = read.csv("/Users/keertisundaram/Dropbox/Data Analytics/lab1/GPW3_GRUMP_SummaryInformation_2010.csv", header = T)

str(GPW3_data)
View(GPW3_data)
summary(GPW3_data)
attach(GPW3_data)

#filtering
Pop <- is.na(PopulationPerUnit)
PopPerUnit <- PopulationPerUnit[!Pop]

#exploration and summary
fivenum(PopPerUnit, na.rm=TRUE)
stem(PopPerUnit)
hist(PopPerUnit)
hist(PopPerUnit, seq(30., 95., 1.0), prob=TRUE) 
lines(density(PopPerUnit, na.rm=TRUE, bw=1.))
rug(PopPerUnit)

#plotting
plot(ecdf(PopPerUnit), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(PopPerUnit)
qqline(PopPerUnit)
summary(GPW3_data)

#comparing distributions
boxplot(PopPerUnit, NumUnits, main = "Population Per Unit and NumUnits")
