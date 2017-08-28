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
split_up    = list()  # The k-folds (same to dataset.tst)
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

# Create lists for useful sets
dataset.tra    = list()  # Training set
dataset.tra.cl = list()  #  |-> Class
dataset.tra.dt = list()  #  \-> Data
dataset.tst    = list()  # Test set
dataset.tst.cl = list()  #  |-> Class
dataset.tst.dt = list()  #  \-> Data
for (k in 1:10) {
  dataset.tra[[k]]    = dataset.train(k)
  dataset.tra.dt[[k]] = subset((dataset.train(k)), select = -class)
  dataset.tra.cl[[k]] = dataset.tra[[k]]$class
  dataset.tst[[k]]    = dataset.test(k)
  dataset.tst.dt[[k]] = subset((dataset.test(k)), select = -class)
  dataset.tst.cl[[k]] = dataset.tst[[k]]$class
}

# Get a sample of the dataset (one fold is 10%, already many points)
dataset.sample = dataset.test(1)

# Separate data into numeric and factor (useful for visualization)
dataset.numeric = dataset[,sapply(dataset, class) != "factor"]
dataset.factor  = dataset[,sapply(dataset, class) == "factor"]
dataset.sample.numeric = dataset.sample[,sapply(dataset, class) != "factor"]
dataset.sample.factor  = dataset.sample[,sapply(dataset, class) == "factor"]

# Remove dummy variables
rm(k, filename, x)
