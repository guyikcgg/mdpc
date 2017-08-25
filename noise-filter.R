


library(NoiseFiltersR)

set.seed(1)
out = IPF(
  formula = class~., 
  data = cbind(data.numeric.simplified, class = data$class),
  nfolds = 10
)

summary(out, explicit = T)
identical(out$cleanData, data.numeric.simplified[setdiff(1:nrow(data.numeric.simplified), out$remIdx), ])


ggplot(out$cleanData) + geom_bar(aes(class, fill = class))


dim(out$cleanData)

p = sum(out$cleanData$class == "positive")
n = sum(out$cleanData$class == "negative")
p/n # Most of the positive examples are removed!!

p = sum(data$class == "positive")
n = sum(data$class == "negative")
p/n


