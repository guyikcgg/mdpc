################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: outlier-removal.R                                      #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(outliers)
library(FSelector)
library(mvoutlier)

# Visualization
if (!exists("plot.enable")) {
  plot.enable = F
}

set.seed(1)

# A row will be considered an outlier if it presents an
# anomaly on any variable or if it presents an anomalous
# combination of its values.

# UNIVARIATE OUTLIERS

# Remove univariate outliers (only from numeric data, not integers!)

## Look for outliers in every column
my.dataset.dt = dataset.numeric.dt[, sapply(dataset.numeric.dt, class) == "numeric"]
my.dataset.cl = dataset.numeric.cl
my.dataset    = cbind(my.dataset.dt, class = my.dataset.cl)
removed.rows  = 0
my.selection  = c()
for (i in 1:ncol(my.dataset.dt)) {
    new.data     = rm.outlier(my.dataset.dt[,i], fill = F)
    my.selection = c(my.selection, which(!(my.dataset.dt[,i] %in% new.data)))
    
    if (plot.enable) {
      # Visualize the removed outliers
      print(
        ggplot(my.dataset, aes_string(
          (names(my.dataset)[i]),
          color = "class")) +
          geom_freqpoly(bins = 70) +
          geom_freqpoly(data = dataset.numeric[my.selection, ], color = "#FFCC00", bins = 70)
      )
    }
    
}

## Get the rows containing outliers
my.selection  = unique(my.selection)
removed.rows  = length(my.selection)

## Remove the rows containing outliers
my.dataset    = my.dataset[-my.selection, ]
my.dataset.cl = dataset.numeric.cl[-my.selection]

print(paste("Removed", removed.rows, "rows containing univariate outliers"))



# MULTIVARIATE OUTLIERS

# Remove multivariate outliers (from both numeric and integer)

## Select the 10 most useful variables according to chi.squared test
weights.chi.squared = chi.squared(class~., dataset.numeric)
weights.chi.squared =
  weights.chi.squared[order(weights.chi.squared, decreasing = T), , drop = F]

my.selection = names(dataset.numeric.dt) %in% rownames(weights.chi.squared)[1:10]
my.dataset.10.dt = dataset.numeric.dt[,my.selection]
my.dataset.10.cl = dataset.numeric.cl
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

## Remove these outliers from the data
my.dataset.10.clean.dt = my.dataset.10.dt[-MCD.outliers, ]
my.dataset.10.clean.cl = my.dataset.10.cl[-MCD.outliers]
my.dataset.10.clean = cbind(my.dataset.10.clean.dt, class = my.dataset.10.clean.cl)

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


## Check that the removal of outliers is independent to the class the data come from
new.proportion = table(my.dataset.10.clean.cl)
old.proportion = table(dataset$class)

prop.table(new.proportion)
prop.table(old.proportion)
## Proportions are very similar, i.e. the removal of registers is not conditioned by the class
## (it could be thought that the minoritary class would be selected for removals)


# Analyze the percentage of removed registers
1 - nrow(my.dataset.10.clean) / nrow(my.dataset)
## Around 17% of the registers have been removed
