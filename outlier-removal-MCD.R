################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: outlier-removal-MCD.R                                  #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(FSelector)
library(mvoutlier)
library(reshape)
library(ggplot2)

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

# A row will be considered an outlier if it presents an
# anomaly on any variable or if it presents an anomalous
# combination of its values.



# MULTIVARIATE OUTLIERS

# Remove multivariate outliers (from both numeric and integer)

## Select the 10 most useful variables according to chi.squared test
weights.chi.squared = chi.squared(class~., subset.num(dataset.tra))
weights.chi.squared =
  weights.chi.squared[order(weights.chi.squared, decreasing = T), , drop = F]

my.selection = names(subset.numeric.dt(dataset.tra)) %in% rownames(weights.chi.squared)[1:10]
my.dataset.10.dt = subset.numeric.dt(dataset.tra)[,my.selection]
my.dataset.10.cl = subset.numeric.cl(dataset.tra)
my.dataset.10    = cbind(my.dataset.10.dt, class = my.dataset.10.cl)

## Normalize the data
my.dataset.10.dt.scaled = scale(my.dataset.10.dt)

## Get the multivariate outliers
alpha.value = 0.05
alpha.value = 1 - ( 1 - alpha.value) ^ (1 / nrow(my.dataset.10.dt.scaled))
mvoutlier.plot = uni.plot(my.dataset.10.dt.scaled, symb = FALSE, alpha = alpha.value)
is.MCD.outlier = mvoutlier.plot$outliers

## Get the rows containing outliers
MCD.outliers = which(is.MCD.outlier)

removed.rows = length(MCD.outliers)
print(paste("Removed", removed.rows, "rows containing multivariate outliers"))

## Visualize removed outliers
if (plot.enable) {
  ggplot(my.dataset.10, aes(X1, X3, color = class)) +
    geom_point(alpha = 0.1) +
    geom_point(data = my.dataset.10[MCD.outliers, ], alpha = 0.1, color = "#FFCC00")
  ggplot(my.dataset.10, aes(X3, X40, color = class)) +
    geom_jitter(alpha = 0.1) +
    geom_jitter(data = my.dataset.10[MCD.outliers, ], alpha = 0.1, color = "#FFCC00")
}

