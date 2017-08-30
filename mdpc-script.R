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

# Visualization
if (!exists("plot.enable")) {
  plot.enable = F
}

# Preprocessing
source("load-data.R")
if (plot.enable) {
  source("initial-visualization.R")
}
source("outlier-removal.R")
source("discretization.R")


# Classification
