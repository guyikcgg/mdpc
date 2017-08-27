


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
prop.table(table(out$cleanData$class)) #BAD!






source("load-data.R")
source("attribute-selection.R")
source("outlier-removal.R")

library(unbalanced)

# Remove some points with ENN (Wilson 1972) # Try Tomek?
result = ubENN(my.data.10.clean, ifelse(my.data.10.clean.cl == "negative", 0, 1), verbose = T)
prop.table(table(result$Y)) #OK!

# Retrieve filtered data
my.data.10.cleaner    = my.data.10.clean[-result$id.rm, ]
my.data.10.cleaner.cl = my.data.10.clean.cl[-result$id.rm]



# Create some variables to plot
pqn = my.data.10.clean
names(pqn) = c(paste("X", 1:ncol(pqn), sep = ""))

my.class = factor(c(my.data.10.clean.cl), levels = c(1,2,3), labels = c("negative", "positive", "noise"))
my.class[result$id.rm] = "noise"



# Some plots demonstrating that the removed points were indeed strange
# (only points from the negative class have been removed)
ggplot(pqn, aes(X2, X3, color = my.data.10.clean.cl)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = pqn[result$id.rm, ], alpha = 0.5, color = "#FFCC00")

ggplot(pqn, aes(X3, X7, color = my.data.10.clean.cl)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = pqn[result$id.rm, ], alpha = 0.5, color = "#FFCC00")

ggplot(pqn, aes(X2, X8, color = my.data.10.clean.cl)) + 
  geom_point(alpha = 0.1) +
  geom_point(data = pqn[result$id.rm, ], alpha = 0.5, color = "#FFCC00")

ggplot(pqn, aes(X1, X5, color = my.data.10.clean.cl)) + 
  geom_jitter(alpha = 0.1) +
  geom_jitter(data = pqn[result$id.rm, ], alpha = 0.5, color = "#FFCC00")
