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

#Chapter 3
library(gcookbook)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
BOD
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
#converting time to a discrete (categorical) var using factor()
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", fill="pink", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")

cabbage_exp
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) +geom_bar(position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity")
ggplot(diamonds, aes(x=cut)) + geom_bar()
data("diamonds")
diamonds
ggplot(diamonds,aes(x=carat)) + geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()

ups <- subset(uspopchange, rank(Change)>40)
ups
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()

ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") + scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") + scale_fill_manual(values=c("#224455","#DDCC33"))

csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >= 0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))
ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=-0.2) + ylim(0, max(cabbage_exp$Weight) * 1.05)
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(y=Weight+0.1, label=Weight))
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=Weight), vjust=1.5, colour="white", position=position_dodge(.9), size=3)

tophit <- tophitters2001[1:25,]
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + theme_bw() +theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) + geom_segment(aes(yend=name), xend=0, colour="grey70")+ geom_point(size=3, aes(colour=lg)) + scale_color_brewer(palette="Set1", limits=c("NL","AL")) +theme_bw() + theme(panel.grid.major.y = element_blank(), legend.position = c(1,0.55), legend.justification = c(1,0.5))
ggplot(tophit, aes(x=avg, y=name)) + geom_segment(aes(yend=name), xend=0, colour="grey40") + geom_point(size=3, aes(colour=lg)) + scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) + facet_grid(lg ~ ., scales = "free_y", space="free_y")
