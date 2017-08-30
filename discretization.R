################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: discretization.R                                       #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(discretization)
library(ggplot2)

# Visualization
if (!exists("plot.enable")) {
  plot.enable = F
}

# Set the random seed in order to be able to reproduce the results
set.seed(1)

# Discetize the data
dataset.discretized = disc.Topdown(
  subset.numeric(dataset.tra.preprocessed)
)

# Plot the results
if (plot.enable) {
  ## Select 10 most meaningful attributes for plot (according to chi.squared test)
  weights.chi.squared = chi.squared(class~., subset.numeric(dataset.tra))
  weights.chi.squared =
    weights.chi.squared[order(weights.chi.squared, decreasing = T), , drop = F]
  
  my.selection = names(subset.numeric.dt(dataset.tra.preprocessed)) %in% rownames(weights.chi.squared)[1:10]
  my.dataset.10.dt = subset.numeric.dt(dataset.tra.preprocessed)[,my.selection]
  my.dataset.10.cl = subset.numeric.cl(dataset.tra.preprocessed)
  my.dataset.10    = cbind(my.dataset.10.dt, class = my.dataset.10.cl)
  my.dataset.10.discretized.dt = subset.numeric.dt(dataset.discretized$Disc.data)[,my.selection]
  my.dataset.10.discretized.cl = subset.numeric.cl(dataset.discretized$Disc.data)
  my.dataset.10.discretized    = cbind(my.dataset.10.discretized.dt, class = my.dataset.10.cl)
  cuts.10 = dataset.discretized$cutp[my.selection]
  
  
  ## Plot the discretized data (only 10 most meaningful variables)
  myData = melt.data.frame(
    my.dataset.10.discretized, 
    id.vars = "class"
  )
  
  ggplot(myData, aes(value, fill = class)) +
    geom_bar(position = "dodge") +
    facet_wrap(~variable, scales = "free", ncol = 5)
  
  
  ## Plot the cut points, along with preprocessed data distributions
  ## (only 10 most meaningful variables)
  myData = melt.data.frame(
    my.dataset.10,
    id.vars = "class"
  )
  myCuts = data.frame(
    variable = rep(
      names(my.dataset.10)[1:10], 
      each = 3
    ), 
    value = unlist(
      cuts.10
    )
  )
  ggplot(myData) +
    geom_freqpoly(aes(value), bins = 50) +
    geom_freqpoly(aes(value, color = class), bins = 50) +
    geom_vline(
      data = myCuts, 
      aes(xintercept = value), 
      color = "#BB1199",
      alpha = 0.5
    ) +
    facet_wrap(~variable, scales = "free", ncol = 2)
}
## As it can be seen in the plots, the cuts are made where the 
## possitive/negative proportion changes the most


# Give a proper name to output from this step
dataset.tra.preprocessed.discretized = dataset.discretized


# Remove temporal variables
rm(
  dataset.discretized, 
  myData,
  myCuts,
  cuts.10,
  my.selection,
  my.dataset.10.discretized,
  my.dataset.10.discretized.cl,
  my.dataset.10.discretized.dt,
  my.dataset.10,
  my.dataset.10.cl,
  my.dataset.10.dt
)
