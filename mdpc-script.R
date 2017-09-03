################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: mdpc-script.R                                          #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Clear workspace
closeAllConnections()
rm(list=ls())

# Visualization option
if (!exists("plot.enable")) {
  plot.enable = F
}

# Remove variables option
if (!exists("rm.variables")) {
  rm.variables = F
}

# Load options
source("mdpc-options.R")

# Load data
source("load-data.R")

# Change the default training and test sets
k = mdpc.options$data.load$k

dataset.tra = dataset.train(k)
dataset.tra.cl = dataset.tra$class
dataset.tra.dt = subset(dataset.tra, select = -class)
dataset.tst = dataset.test(k)
dataset.tst.cl = dataset.tst$class
dataset.tst.dt = subset(dataset.tst, select = -class)

# Visualization
if (plot.enable) {
  source("initial-visualization.R")
}

# Preprocessing
if (!is.na(mdpc.options$preprocessing$outliersUNI)) {
  source("outlier-removal-UNI.R")
}

if (!is.na(mdpc.options$preprocessing$outliersMCD)) {
  source("outlier-removal-MCD.R")
}

source("outlier-removal.R")


if (!is.na(mdpc.options$preprocessing$discretization)) {
  source("discretization.R")
}

if (!exists("dataset.tra.preprocessed")) {
  dataset.tra.preprocessed = dataset.tra
  dataset.tra.preprocessed.dt = dataset.tra.dt
  dataset.tra.preprocessed.cl = dataset.tra.cl
}

if (!is.na(mdpc.options$preprocessing$attribute.selection)) {
  source("attribute-selection.R")
}


if (!is.na(mdpc.options$preprocessing$noise.filter)) {
  source("noise-filter.R")
}

# Classification
if (!is.na(mdpc.options$preprocessing$noise.filter)) {
  if (mdpc.options$preprocessing$noise.filter == "IPF") {
    training.set = IPF.out$cleanData
  } else if (mdpc.options$preprocessing$noise.filter == "ENN") {
    training.set = my.clean.data
  }
} else if (!is.na(mdpc.options$preprocessing$discretization)) {
  training.set = dataset.tra.preprocessed.discretized
} else if (!is.na(mdpc.options$preprocessing$attribute.selection)) {
  cor.cutoff = mdpc.options$preprocessing$attribute.selection
  training.set = select.attributes(dataset.tra.preprocessed, cor.cutoff)
} else {
  training.set = dataset.tra.preprocessed
}

test.set = dataset.tst

class.opts = mdpc.options$classification

source("classification-orig.R")

if (!is.na(class.opts$down)) {
  source("classification-down.R")
}

if (!is.na(class.opts$up)) {
  source("classification-up.R")
}

if (!is.na(class.opts$smote)) {
  source("classification-smote.R")
}

# Visualize the results
if (plot.enable) {
  source("classification-visualization.R")
}
