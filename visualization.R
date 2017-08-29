#Visualization

source("load-data.R")

library('caret')
data(iris)
formula <- as.formula(Species ~.)
t <- caret::train(formula,iris,method = "rpart",cp=0.002,maxdepth=8)
plot(t$finalModel)

t <- caret::train(class ~ .,
                  data = data,
                  method = "rpart",
                  cp=0.002,maxdepth=8)


plot(t$finalModel)
text(t$finalModel)

library(rattle)
fancyRpartPlot(t$finalModel)
