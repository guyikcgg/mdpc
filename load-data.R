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


# Load traning data and test data
data.tra    = list()  # Training data
data.tra.cl = list()  #  |-> Class
data.tra.dt = list()  #  \-> Data
data.tst    = list()  # Test data
data.tst.cl = list()  #  |-> Class
data.tst.dt = list()  #  \-> Data
for (i in 1:10) {
  for (j in 1:2) {
    filename = paste(
      "data/datcom2016-10-",
      as.character(i),
      ifelse(j==1, "tra", "tst"),
      ".csv",
      sep = ""
    )
    x = read.csv(filename)
    rownames(x) = x$X
    x = x[,-1]
    
    if(j==1) {
      data.tra[[i]]    = x
      data.tra.dt[[i]] = subset(x, select = -class)
      data.tra.cl[[i]] = x$class
    } else {
      data.tst[[i]]    = x
      data.tst.dt[[i]] = subset(x, select = -class)
      data.tst.cl[[i]] = x$class
    }
  }
}

# Remove dummy variables
rm(i, j, filename, x)