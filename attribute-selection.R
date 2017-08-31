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


# Get a score for the importance of attributes according to the information they provide

numeric.data.dt = subset.numeric.dt(dataset.tra.preprocessed)
numeric.data = subset.numeric(dataset.tra.preprocessed)
weights = data.frame(attribute = names(dataset.tra.preprocessed)[1:50])
weights.num = data.frame(attribute = names(numeric.data.dt))

## According to their independence to the class variable
weights$chi.squared = 
  chi.squared(class~., dataset.tra.preprocessed)

weights.chi.squared = 
  weights$chi.squared[
    order(weights$chi.squared, decreasing = T), , drop = F
  ]

## These results can be verified with a KS test, by checking 
## for similar distributions between the positive and 
## negative class
ks.test.results.pn = matrix(
  nrow = ncol(numeric.data.dt), 
  ncol = 1
)
rownames(ks.test.results.pn) = names(numeric.data.dt)
myData.p = subset(
  numeric.data,
  subset = class == "positive"
)
myData.n = subset(
  numeric.data,
  subset = class == "negative"
)
for (i in 1:(ncol(numeric.data.dt))) {
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


## Entropy measures
weights$information.gain = information.gain(class~., dataset.tra.preprocessed, "log2")
weights.num$information.gain = information.gain(class~., numeric.data, "log2")
weights.information.gain = weights$information.gain[order(weights$information.gain, decreasing = T), , drop = F]

weights$gain.ratio = gain.ratio(class~., dataset.tra.preprocessed, "log2")
weights.num$gain.ratio = gain.ratio(class~., numeric.data, "log2")
weights.gain.ratio = weights$gain.ratio[order(weights$gain.ratio, decreasing = T), , drop = F]
### Results differ greatly!!


# Check for redundancies
## Check correlation amongst variables
correlations = cor(numeric.data.dt)
findCorrelation(correlations, cutoff = 0.8)
drop.attributes.n = c(
  drop.attributes.n,
  findCorrelation(correlations, cutoff = 0.8)
)






weights$oneR = 1-oneR(class~., dataset.tra.preprocessed)
weights.oneR = weights$oneR[order(weights$oneR, decreasing = T), , drop = F]
## Numerical variables have been discretized. 


my.scale = function(x) { return ((x-min(x))/(max(x)-min(x))) }

weights.scaled = weights
weights.scaled[,2:ncol(weights)] = data.frame(lapply(weights[,2:ncol(weights)], my.scale ))
weights.scaled$sum = rowSums(weights.scaled[,2:ncol(weights)])
weights.scaled$sum.scaled = 1.5*my.scale(weights.scaled$sum)
weights.scaled$type = sapply(dataset, class)[1:50]
weights.scaled = weights.scaled[order(weights.scaled$sum, decreasing = T), , drop = F]
weights.scaled = weights.scaled[1:40, ]
rownames(weights.scaled) = 1:40


myData = melt.data.frame(
  weights.scaled,
  measure.vars = c("sum.scaled", "chi.squared", "oneR", "gain.ratio", "information.gain")
)
myData$attribute = factor(myData$attribute, levels = weights.scaled$attribute)

ggplot(myData) + 
  geom_rect(
    aes(color = type),
    xmin = -Inf,
    xmax = Inf, 
    ymin = -Inf,
    ymax = Inf, 
    alpha = 0.02
  ) +
  geom_col(aes(variable, value, fill = variable)) + 
  facet_wrap(~attribute, ncol = 5) + coord_flip() +
  xlab("") + ylab("")
  










# Drop completely irrelevant numeric attributes
drop.attributes = 
  rownames(weights.chi.squared)[weights.chi.squared == 0]
drop.attributes.n = which(names(numeric.data) %in% drop.attributes)




# Drop the attributes
drop.attributes.n = unique(drop.attributes.n)
print(paste("Dropping", length(drop.attributes.n), "attributes..."))
numeric.data.simplified = 
  numeric.data[, -drop.attributes.n]




stop()







