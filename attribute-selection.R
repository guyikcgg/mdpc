################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: attribute-selection.R                                  #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(utils)
library(ggplot2)
library(reshape)
library(FSelector)
library(caret)

# Load the data
source("load-data.R")

# Set the random seed in order to be able to reproduce the results
set.seed(1)

# Get a score for the importance of attributes

## According to their independence to the class variable
weights.chi.squared = FSelector::chi.squared(class~., data)
weights.chi.squared = 
  weights.chi.squared[order(weights.chi.squared, decreasing = T), , drop = F]

## These results can be verified with a KS test, by checking 
## for similar distributions between the positive and 
## negative class
ks.test.results.pn = matrix(
  nrow = ncol(data.numeric), 
  ncol = 1
)
rownames(ks.test.results.pn) = names(data.numeric)
myData.p = subset(cbind(data.numeric, class = data$class), subset = class == "positive")
myData.n = subset(cbind(data.numeric, class = data$class), subset = class == "negative")
for (i in 1:(ncol(data.numeric))) {
  ks.test.results.pn[i,1] = ks.test(
    myData.p[,i], 
    myData.n[,i]
  )$p.value
}

rownames(ks.test.results.pn)[ks.test.results.pn>0.1]

## In conclusion: having the same distribution for positive and negative is not very informative...
## We will drop every variable getting 0 score in chi.squared test.

# Drop completely irrelevant numeric attributes
drop.attributes = 
  rownames(weights.chi.squared)[weights.chi.squared == 0]
drop.attributes = which(names(data.numeric) %in% drop.attributes)


# Look for redundant attributes
## Check correlation amongst variables
correlations = cor(data.numeric)
drop.attributes = c(
  drop.attributes,
  findCorrelation(correlations, cutoff = 0.8)
)

# Drop the attributes
drop.attributes = unique(drop.attributes)
print(paste("Dropping", length(drop.attributes), "attributes..."))
data.numeric.simplified = 
  data.numeric[, -drop.attributes]




stop()




weights.correlation.linear = FSelector::linear.correlation(data.numeric)
weights.correlation.linear = 
  weights.correlation.linear[order(weights.correlation.linear, decreasing = T), , drop = F]

weights.correlation.rank = FSelector::rank.correlation(data.numeric)
weights.correlation.rank = 
  weights.correlation.rank[order(weights.correlation.rank, decreasing = T), , drop = F]

rownames(weights.correlation.rank) == rownames(weights.correlation.linear)
## Same order of importance in first 3 records


weights.information.gain = information.gain(class~., data)
gain = weights.information.gain[order(weights.information.gain, decreasing = T), , drop = F]

## There are other alternatives for information (ratio could be interesting)

weights.oneR = oneR(class~., data)
weights.oneR = weights.oneR[order(weights.oneR, decreasing = F), , drop = F]
## Numerical variables have been discretized. 






