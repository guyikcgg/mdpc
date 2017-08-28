################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: load-data.R                                            #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(utils)
library(stats)


# Load the complete dataset
dataset = read.csv("data/datcom2016.csv")


# Load the 10-folds
split_up    = list()  # The k-folds (same to dataset.test)
for (k in 1:10) {
  filename = paste(
    "data/datcom2016-10-",
    as.character(k),
    "-fold",
    ".csv",
    sep = ""
  )
  x = read.csv(filename)
  rownames(x) = x$X
  x = x[,-1]

  split_up[[k]] = x
}

# Function to quickly access to training set for a given fold
dataset.train = function(i) {
  return(do.call("rbind", split_up[-i]))
}

# Function to quickly access to test set for a given fold
dataset.test = function(i) {
  return(split_up[[i]])
}

# Create some useful sets
dataset.tra    = dataset.train(1)    # Training set
dataset.tra.cl = dataset.tra$class   #  |-> Class
dataset.tra.dt = dataset.tra[, -50]  #  \-> Data
dataset.tst    = dataset.test(1)     # Training set
dataset.tst.cl = dataset.tst$class   #  |-> Class
dataset.tst.dt = dataset.tst[, -50]  #  \-> Data

# Separate data into numeric and factor (useful for visualization)
dataset.numeric = dataset.tra[,sapply(dataset, class) != "factor"]
dataset.factor  = dataset.tra[,sapply(dataset, class) == "factor"]

# Remove dummy variables
rm(filename, x)
