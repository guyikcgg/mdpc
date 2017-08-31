################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: noise-filter.R                                         #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(NoiseFiltersR)
library(unbalanced)
library(ggplot2)
library(reshape)

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

# Remove noisy instances using IPF
IPF.out = IPF(
  formula = class~., 
  data = dataset.tra.preprocessed.selected,
  nfolds = 10
)

## Get a summary of the noise elimination
summary(IPF.out, explicit = F)

prop.table(table(IPF.out$cleanData$class))
### The noise elimination has affected the positive class, which is the minoritary class.
### We need another approach...



# Remove noise using ENN

# Remove some points with ENN
ENN.out = ubENN(
  subset.numeric.dt(dataset.tra.preprocessed.selected),
  ifelse(
    dataset.tra.preprocessed.selected.cl == "negative",
    0, 
    1
  ), 
  verbose = T
)

## Get a summary of the results
print(sprintf(
  "Removed %d noisy instances (%.2f %% of the total)",
  nrow(dataset.tra.preprocessed.selected) - nrow(ENN.out$X),
  100 * ( 1 - nrow(ENN.out$X) / nrow(dataset.tra.preprocessed.selected))
))

## Retrieve filtered data
removed.instances = ENN.out$id.rm
my.clean.data    = dataset.tra.preprocessed.selected[-removed.instances, ]
my.clean.data.dt = dataset.tra.preprocessed.selected.dt[-removed.instances, ]
my.clean.data.cl = dataset.tra.preprocessed.selected.cl[-removed.instances]

## Check the relative frequencies of each class
print(
  prop.table(table(my.clean.data.cl))
)

### The proportion has improved (as expected)


# Some plots demonstrating that the removed points were indeed strange
# (only points from the negative class have been removed)
if (plot.enable) {
  ggplot(dataset.tra.preprocessed.selected, aes(X2, X3, color = class)) + 
    geom_point(alpha = 0.1) +
    geom_point(data = dataset.tra.preprocessed.selected[removed.instances,], alpha = 0.5, color = "#FFCC00")
  
  ggplot(dataset.tra.preprocessed.selected, aes(X1, X5, color = class)) + 
    geom_jitter(alpha = 0.1) +
    geom_jitter(data = dataset.tra.preprocessed.selected[removed.instances,], alpha = 0.5, color = "#FFCC00")
  
  ggplot(dataset.tra.preprocessed.selected, aes(X1, X2, color = class)) + 
    geom_point(alpha = 0.1) +
    geom_point(data = dataset.tra.preprocessed.selected[removed.instances,], alpha = 0.5, color = "#FFCC00")
  
  ggplot(dataset.tra.preprocessed.selected, aes(X2, X8, color = class)) + 
    geom_jitter(alpha = 0.1) +
    geom_jitter(data = dataset.tra.preprocessed.selected[removed.instances,], alpha = 0.5, color = "#FFCC00")
}


if (rm.variables) {
  rm(
    ENN.out,
    IPF.out,
    removed.instances
  )
}
