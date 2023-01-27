EPI_data <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/lab1/EPI2010_data.csv", header=T, skip = 1)

EPI_data
View(EPI_data)
attach(EPI_data)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)

help(qqnorm)
par(pty="s")
qqnorm(EPI)
qqline(EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE)
#can see points if do.points = TRUE
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")

help(qqplot)
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)

x
#doesn't include 95 since that is not within an increment of 2 
x2 <-seq(30,95,2)
x2
#inclusive of 96, goes up by increments of 2 starting at 30 ending at 96
x2 <-seq(30,96,2)
x2

qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

#Different variables
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(DALY)
qqline(DALY)

plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(BIODIVERSITY)
qqline(BIODIVERSITY)

boxplot(EPI_data$EPI,EPI_data$DALY)

#Comparing distributions
boxplot(EPI, ENVHEALTH, main = "EPI and ENVHEALTH")
boxplot(EPI, ECOSYSTEM,  main = "EPI and ECOSYSTEM")
boxplot(EPI, DALY,  main = "EPI and DALY")
boxplot(EPI, AIR_H,  main = "EPI and AIR_H")
boxplot(EPI, WATER_H,  main = "EPI and WATER_H")
boxplot(EPI, AIR_E,  main = "EPI and AIR_E")
boxplot(EPI, WATER_E,  main = "EPI and WATER_E")
boxplot(EPI, BIODIVERSITY,  main = "EPI and BIODIVERSITY")

multivariate <- read.csv("/Users/keertisundaram/Dropbox/Data Analytics/lab 1b/multivariate.csv")
attach(multivariate)
head(multivariate)
summary(multivariate)
str(multivariate)
help(lm)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm, col=6, lwd=3)

library(dplyr)
newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) 
mm$coefficients

#ggplot examples
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
#different notation, same plot
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data = pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()+geom_point()
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()+geom_point()

#bar graphs
barplot(BOD$demand, names.arg= BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data=mtcars)
#same bar graph as line 106
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

#histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)
hist(mtcars$mpg, breaks=5)
hist(mtcars$mpg, breaks=12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=5)

#box plots
plot(ToothGrowth$supp, ToothGrowth$len)
#if in same data frame
boxplot(len ~ supp, data= ToothGrowth)
boxplot(len ~ supp + dose, data= ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom= "boxplot")
qplot(supp, len, data = ToothGrowth, geom= "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
