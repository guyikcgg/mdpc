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
data = read.csv("data/datcom2016.csv")


# Load the 10-folds
split_up    = list()  # The k-folds (same to data.tst)
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

# Function to quickly access to training data for a given fold
data.train = function(i) {
  return(do.call("rbind", split_up[-i]))
}

# Function to quickly access to test data for a given fold
data.test = function(i) {
  return(split_up[[i]])
}

# Create lists for useful data
data.tra    = list()  # Training data
data.tra.cl = list()  #  |-> Class
data.tra.dt = list()  #  \-> Data
data.tst    = list()  # Test data
data.tst.cl = list()  #  |-> Class
data.tst.dt = list()  #  \-> Data
for (k in 1:10) {
  data.tra[[k]]    = data.train(k)
  data.tra.dt[[k]] = subset((data.train(k)), select = -class)
  data.tra.cl[[k]] = data.tra[[k]]$class
  data.tst[[k]]    = data.test(k)
  data.tst.dt[[k]] = subset((data.test(k)), select = -class)
  data.tst.cl[[k]] = data.tst[[k]]$class
}

# Get a sample of the data (one fold is 10%, already many points)
data.sample = data.test(1)

# Separate data into numeric and factor (useful for visualization)
data.numeric = data[,sapply(data, class) != "factor"]
data.factor  = data[,sapply(data, class) == "factor"]
data.sample.numeric = data.sample[,sapply(data, class) != "factor"]
data.sample.factor  = data.sample[,sapply(data, class) == "factor"]

# Remove dummy variables
rm(k, filename, x)
