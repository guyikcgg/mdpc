################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: initial-visualization.R                                #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################


# Load required libraries
library(utils)
library(ggplot2)
library(reshape)

# Load the dataset
dataset = read.csv("data/datcom2016.csv")

# Set the random seed in order to be able to reproduce the results
set.seed(1)


# Visualize the structure of the data
dim(dataset)

data.types = sapply(dataset[, -51], class)
table(data.types)
## Data contains 20000 registers from 50 attributes plus a class variable.
## 9 of the attributes are factors, whist the remaining 41 are numeric (
## 22 of which are integer, the other 19 being continous variables)


# Build a summary table for the document
mt = matrix(nrow = 50, ncol = 4)
mt[,1] = names(data.types)
mt[,2] = data.types
mt[,4] = paste("X", 1:50, sep ="")
for (i in 1:50) {
  if (data.types[i] == "factor") {
    mt[i,3] = paste("{", paste(levels(dataset[,i]), collapse = ", "), "}", sep = "")
  } else if (data.types[i] == "numeric") {
    mt[i,3] = sprintf("[%.2f, %.2f]", min(dataset[,i]), max(dataset[,i]))
  } else {
    mt[i,3] = sprintf("[%d, %d]", min(dataset[,i]), max(dataset[,i]))
  }
}


# Get additional summary of the data
str(dataset)
summary(dataset)

## There are too many variables to understand at once. Let's do it
## little by little.

# Frequency of each class
table(dataset$class)
prop.table(table(dataset$class))

# Plot frequency of each class
ggplot(dataset) +
  geom_bar(aes(class, fill = class))

## We are facing an imbalanced problem


# Check for missing values
data.frame(missing.values = colSums(is.na(dataset)))

## There are no missing values


# Plot numeric data
## Distributions
dataset.numeric.dt = dataset[,sapply(dataset, class) != "factor"]
dataset.numeric.cl = dataset$class
dataset.numeric = cbind(
  dataset.numeric.dt,
  class = dataset.numeric.cl
)
dataset.factor  = dataset[,sapply(dataset, class) == "factor"]
myData = melt.data.frame(dataset.numeric)

## Density (relative to the class)
ggplot(myData) +
  geom_density(aes(value)) +
  geom_density(aes(value, color = class)) +
  facet_wrap(~variable, scales = "free")
## Frequency (more samples, more frequency)
ggplot(myData) +
  geom_freqpoly(aes(value)) +
  geom_freqpoly(aes(value, color = class)) +
  facet_wrap(~variable, scales = "free")


# Plots for the document
myData1 = melt.data.frame(
  cbind(dataset.numeric[,1:40], class = dataset.numeric.cl)
)
myData2 = melt.data.frame(
  dataset.factor,
  id.vars = "class"
)
ggplot(myData1) +
  geom_freqpoly(aes(value)) +
  geom_freqpoly(aes(value, color = class)) +
  facet_wrap(~variable, scales = "free", ncol = 4)
ggplot(myData2) +
  geom_bar(aes(value, fill = class), position = "dodge") +
  facet_wrap(~variable, scales = "free", ncol = 4)


# ATTRIBUTE-SELECTION APPROACH

# Can attributes with the same distribution be informative?
ks.test(dataset$PredCN_r1, dataset$PredCN_r2)
## These two attributes may have the same distribution.
ks.test(
  subset(dataset, subset = class == "positive")$PredCN_r1,
  subset(dataset, subset = class == "positive")$PredCN_r2
)
ks.test(
  subset(dataset, subset = class == "negative")$PredCN_r1,
  subset(dataset, subset = class == "negative")$PredCN_r2
)
## Indeed, their relationship to positive and negative classes
## seem to be the same.
myData = subset(dataset, select = c(PredCN_r1, PredCN_r2, class))
myData = melt(myData)
ggplot(myData) +
  geom_density(aes(value)) +
  geom_density(aes(value, color = class)) +
  facet_wrap(~variable, ncol = 1)
## It could be thought that one of them could be redundant, thus
## being appropriate to omit it.
ggplot(dataset) + geom_bar(aes(class, fill = as.factor(PredCN_r1)))
ggplot(dataset) + geom_bar(aes(class, fill = as.factor(PredCN_r2)))
## They do seem to hold the same information.
cor(dataset$PredCN_r1, dataset$PredCN_r2)
## Nevertheless, the two variables are not correlated...
ggplot(dataset) +
  geom_jitter(aes(PredCN_r1, PredCN_r1, color = class), alpha = 0.1)
ggplot(dataset) +
  geom_jitter(aes(PredCN_r1, PredCN_r2, color = class), alpha = 0.1)
## And that is the most important part, because they are independent,
## but they inform very well about the class.
## Having the two variables makes a great decission boundary,
## so none of them should be deleted.
## Conclusion: same distribution does not mean the variables should be removed...
