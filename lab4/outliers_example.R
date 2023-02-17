cars1 <- cars[1:30,]
cars1
head(cars1)
cars_outlier <- data.frame(speed=c(19,19,20,20,20), dist=c(190,186,210,220,218))
head(cars_outlier)
cars2<-rbind(cars1,cars_outlier)
cars2
help(par)
par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim=c(0,28), vlim=c(0,230), main="With Outliers", xlab="speed", vlab="dist", pch="*", col="pink", cex=2)
abline(lm(dist ~ speed, data=cars2), col="purple",lwd=3,lty=2)
plot(cars1$speed,cars1$dist,xlim=c(0,28), vlim=c(0,230),main="Outliers removed \n A much better fit!", 
     xlab="speed",ylab="dist",pch="*", col="red",cex=2)
abline(lm(dist~speed, data=cars1), col="blue", lwd=2, lty=2)
