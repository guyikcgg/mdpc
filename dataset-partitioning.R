################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: dataset-partitioning.R                                 #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(utils)
library(caret)

# Load the dataset
dataset = read.csv("data/datcom2016.csv")

# Set the random seed in order to be able to reproduce the results
set.seed(1)

# Create the folds for k-fold cross validation
folds = createFolds(
  y = dataset$class,
  k = 10
)

# Get the folds from dataset
split_up = lapply(
  folds, 
  function(ind, dat) {
    dat[ind,]
  },
  dat = dataset
)

# Quick check for the structure
unlist(lapply(split_up, nrow))

# Function to quickly access to training data for a given fold
dataset.train = function(i) {
  return(do.call("rbind", splidatat_up[-i]))
}

# Function to quickly access to test data for a given fold
dataset.test = function(i) {
  return(split_up[[i]])
}

# Save the data (so it can be loaded afterwards)
for (k in 1:10) {
  write.csv(
    split_up[[k]], 
    paste("data/datcom2016-10-", k, "-fold.csv", sep = "")
  )
}
