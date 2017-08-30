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



# MULTIVARIATE OUTLIERS

# Remove multivariate outliers (from both numeric and integer)

## Select the 10 most useful variables according to chi.squared test
weights.chi.squared = chi.squared(class~., subset.numeric(dataset.tra))
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


# Remove outliers from the data
the.outliers = unique(c(UNI.outliers, MCD.outliers))
dataset.tra.preprocessed    = dataset.tra[-the.outliers, ]
dataset.tra.preprocessed.dt = dataset.tra.dt[-the.outliers, ]
dataset.tra.preprocessed.cl = dataset.tra.cl[-the.outliers]

## Check that the removal of outliers is independent to the class the data come from
new.proportion = table(dataset.tra.preprocessed$class)
old.proportion = table(dataset$class)

print("Class frequency prior outlier removal:")
print(sprintf("negative: %.2f %%", 100*prop.table(old.proportion)[1]))
print(sprintf("positive: %.2f %%", 100*prop.table(old.proportion)[2]))

print("Class frequency after outlier removal:")
print(sprintf("negative: %.2f %%", 100*prop.table(new.proportion)[1]))
print(sprintf("positive: %.2f %%", 100*prop.table(new.proportion)[2]))

## Proportions are very similar, i.e. the removal of registers is not conditioned by the class
## (it could be thought that the minoritary class would be selected for removals)


# Analyze the percentage of removed registers
print(sprintf(
  "Proportion of removed outliers: %.2f %%", 
  100*(1 - nrow(dataset.tra.preprocessed) / nrow(my.dataset))
))
## Around 18% of the registers have been removed

# Remove temporal variables
rm(
  new.proportion,
  old.proportion,
  MCD.outliers,
  UNI.outliers,
  removed.rows,
  the.outliers,
  is.MCD.outlier,
  alpha.value,
  my.selection,
  my.dataset.10,
  my.dataset.10.cl,
  my.dataset.10.dt,
  mvoutlier.plot,
  weights.chi.squared
)
