require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart, main = "swiss r-part") # try some different plot options
text(Swiss_rpart) # try some different text options
