################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: outlier-removal.R                                      #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(outliers)
library(mvoutlier)

# Load data and select the most relevant attributes
source("load-data.R")
source("attribute-selection.R")

# A row will be considered an outlier if it presents an
# anomaly on any variable or if it presents an anomalous
# combination of its values.

# UNIVARIATE OUTLIERS

# Remove univariate outliers (only from numeric data, not integers!)
my.data = data.numeric.simplified
removed.rows = 0
my.selection = c()
for (i in 1:ncol(data.numeric.simplified)) {
  if(class(data.numeric.simplified[,i]) == "numeric") {
    new.data     = rm.outlier(my.data[,i], fill = F)
    my.selection = c(my.selection, which(!(my.data[,i] %in% new.data)))
  }
}

my.selection = unique(my.selection)
my.data      = my.data[-my.selection, ]
removed.rows = length(my.selection)

print(paste("Removed", removed.rows, "rows containing univariate outliers"))
my.data.class = data$class[-my.selection]

# MULTIVARIATE OUTLIERS

# Select the 10 most useful variables according to chi.squared test
my.selection = names(data.numeric.simplified) %in% rownames(weights.chi.squared)[1:10]
my.data.10 = my.data[,my.selection]

# Normalize the data
my.data.10.scaled = scale(my.data.10)

# Get the multivariate outliers
alpha.value = 0.05
alpha.value = 1 - ( 1 - alpha.value) ^ (1 / nrow(my.data.10.scaled))
mvoutlier.plot = uni.plot(my.data.10.scaled, symb = FALSE, alpha = alpha.value)
is.MCD.outlier = mvoutlier.plot$outliers
MCD.outliers = which(is.MCD.outlier)
length(MCD.outliers)

# Remove these outliers from the data
my.data.10.clean = my.data.10[-MCD.outliers, ]

# Check that the removal of outliers is independent to the class the data come from
new.proportion = table(my.data.class[-MCD.outliers])
old.proportion = table(data$class)

new.proportion[2]/new.proportion[1]
old.proportion[2]/old.proportion[1]
## Proportions are very similar, i.e. the removal of registers is not conditioned by the class
## (it could be thought that the minoritary class would be selected for removals)


# Analyze the percentage of removed registers
1 - nrow(my.data.10.clean) / nrow(my.data)
## 17.5% of the registers have been removed
