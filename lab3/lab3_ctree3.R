library(party)
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
help(plot)
plot(fitK, main='Conditional Inference Tree for Kyphosis')

plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")

#etc.

