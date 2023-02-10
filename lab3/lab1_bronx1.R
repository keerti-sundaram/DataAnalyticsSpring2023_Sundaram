library(gdata) 
#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 

#library("xlsx")
#bronx1 <- read.xlsx("/Users/keertisundaram/Downloads/rollingsales_bronx.xls", 1)
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]
#alternate
library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
bronx1<-read.xlsx("/Users/keertisundaram/Downloads/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
str(bronx1)

SP <-log(SALE.PRICE)
SP[is.na(SP) | SP=="-Inf"] = NA

GSF <- log(GROSS.SQUARE.FEET)
GSF[is.na(GSF) | GSF=="-Inf"] = NA
SP
m1<-lm(SP~GSF)

summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2
LSF <- log(bronx1$LAND.SQUARE.FEET)
LSF[is.na(LSF) | LSF=="-Inf"] = NA
m2<-lm(SP~GSF+LSF+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(SP~0+GSF+LSF+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(SP~0+GSF+LSF+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(SP~0+GSF+LSF+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
