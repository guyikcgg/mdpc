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

# Visualization
if (!exists("plot.enable")) {
  plot.enable = F
}

# Remove variables
if (!exists("rm.variables")) {
  rm.variables = F
}

# Set the random seed in order to be able to reproduce the results
set.seed(1)


# Get a score for the importance of attributes

numeric.data = subset.numeric.dt(dataset.tra.preprocessed)

## According to their independence to the class variable
weights.chi.squared = 
  chi.squared(class~., dataset.tra.preprocessed)

weights.chi.squared = 
  weights.chi.squared[
    order(weights.chi.squared, decreasing = T), , drop = F
  ]

## These results can be verified with a KS test, by checking 
## for similar distributions between the positive and 
## negative class
ks.test.results.pn = matrix(
  nrow = ncol(numeric.data), 
  ncol = 1
)
rownames(ks.test.results.pn) = names(numeric.data)
myData.p = subset(
  subset.numeric(dataset.tra.preprocessed),
  subset = class == "positive"
)
myData.n = subset(
  subset.numeric(dataset.tra.preprocessed),
  subset = class == "negative"
)
for (i in 1:(ncol(numeric.data))) {
  ks.test.results.pn[i,1] = ks.test(
    myData.p[,i], 
    myData.n[,i]
  )$p.value
}

## Really non-useful classes
rownames(ks.test.results.pn)[ks.test.results.pn>0.1]

## Verification of the results of chi.squared test
rownames(ks.test.results.pn)[ks.test.results.pn>0.00001]
rownames(weights.chi.squared)[weights.chi.squared == 0]

## In conclusion: having the same distribution for positive and 
## negative is not very informative... We will drop every 
## variable getting 0 score in chi.squared test.

# Drop completely irrelevant numeric attributes
drop.attributes = 
  rownames(weights.chi.squared)[weights.chi.squared == 0]
drop.attributes.n = which(names(numeric.data) %in% drop.attributes)


# Look for redundant attributes
## Check correlation amongst variables
correlations = cor(numeric.data)
drop.attributes.n = c(
  drop.attributes.n,
  findCorrelation(correlations, cutoff = 0.8)
)

# Drop the attributes
drop.attributes.n = unique(drop.attributes.n)
print(paste("Dropping", length(drop.attributes.n), "attributes..."))
numeric.data.simplified = 
  numeric.data[, -drop.attributes.n]




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






