################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: outlier-removal.R                                      #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
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


# Remove outliers from the data
if (!exists("UNI.outliers")) {
  UNI.outliers = c()
}
if (!exists("MCD.outliers")) {
  MCD.outliers = c()
}
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
if (rm.variables) {
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
}
