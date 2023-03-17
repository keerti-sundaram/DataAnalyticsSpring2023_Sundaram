require(rpart)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

#trying with abalone data
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
head(abalone)
#assign column names using concatenation function
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
abalone_rpart <- rpart(rings ~ length + diameter, data = abalone)
plot(abalone_rpart)
text(abalone_rpart)
