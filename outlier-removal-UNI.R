################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: outlier-removal-UNI.R                                  #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(outliers)
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

# UNIVARIATE OUTLIERS

# Remove univariate outliers (only from numeric data, not integers!)

## Look for outliers in every column
my.dataset.dt = subset.numeric.dt(dataset.tra)[, sapply(subset.numeric.dt(dataset.tra), class) == "numeric"]
my.dataset.cl = subset.numeric.cl(dataset.tra)
my.dataset    = cbind(my.dataset.dt, class = my.dataset.cl)
removed.rows  = 0
UNI.outliers  = c()
for (i in 1:ncol(my.dataset.dt)) {
    new.data     = rm.outlier(my.dataset.dt[,i], fill = F)
    UNI.outliers = c(UNI.outliers, which(!(my.dataset.dt[,i] %in% new.data)))
    
    if (plot.enable) {
      # Visualize the removed outliers
      print(
        ggplot(my.dataset, aes_string(
          (names(my.dataset)[i]),
          color = "class")) +
          geom_freqpoly(bins = 70) +
          geom_freqpoly(data = subset.numeric(dataset.tra)[UNI.outliers, ], color = "#FFCC00", bins = 70)
      )
    }
}

## Get the rows containing outliers
UNI.outliers  = unique(UNI.outliers)

removed.rows  = length(UNI.outliers)
print(paste("Removed", removed.rows, "rows containing univariate outliers"))


