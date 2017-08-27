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

# Load the data
source("load-data.R")

# Set the random seed in order to be able to reproduce the results
set.seed(1)




# Visualize the structure of the data
dim(data)
str(data)

sum(sapply(data, class) == "factor") -1
sum(sapply(data, class) == "integer")
sum(sapply(data, class) == "numeric")
## Data contains 20000 registers from 50 attributes plus a class variable.
## 9 of the attributes are factors, whist the remaining 41 are numeric (
## 22 of which are integer, the other 19 being continous variables)

# Get a summary of the data
summary(data)

## There are too many variables to understand at once. Let's do it
## little by little.
### MISSING!! ###


# Plot frequency of each class
ggplot(data) +
  geom_bar(aes(class, fill = class))

## We are facing an imbalanced problem


# Check for missing values
as.data.frame(colSums(is.na(data)))

## There are no missing values


# Plot numeric data
## Distributions
myData = melt.data.frame(
  cbind(data.numeric, class = data$class)
)
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


# ATTRIBUTE-SELECTION APPROACH

# Can attributes with the same distribution be informative?
ks.test(data$PredCN_r1, data$PredCN_r2)
## These two attributes may have the same distribution.
ks.test(
  subset(data, subset = class == "positive")$PredCN_r1,
  subset(data, subset = class == "positive")$PredCN_r2
)
ks.test(
  subset(data, subset = class == "negative")$PredCN_r1,
  subset(data, subset = class == "negative")$PredCN_r2
)
## Indeed, their relationship to positive and negative classes
## seem to be the same.
myData = subset(data, select = c(PredCN_r1, PredCN_r2, class))
myData = melt(myData)
ggplot(myData) +
  geom_density(aes(value)) +
  geom_density(aes(value, color = class)) +
  facet_wrap(~variable, ncol = 1)
## It could be thought that one of them could be redundant, thus
## being appropriate to omit it.
ggplot(data) + geom_bar(aes(class, fill = as.factor(PredCN_r1)))
ggplot(data) + geom_bar(aes(class, fill = as.factor(PredCN_r2)))
## They do seem to hold the same information.
cor(data$PredCN_r1, data$PredCN_r2)
## Nevertheless, the two variables are not correlated...
ggplot(data) +
  geom_jitter(aes(PredCN_r1, PredCN_r1, color = class), alpha = 0.1)
ggplot(data) +
  geom_jitter(aes(PredCN_r1, PredCN_r2, color = class), alpha = 0.1)
## And that is the most important part, because they are independent,
## but they inform very well about the class.
## Having the two variables makes a great decission boundary,
## so none of them should be deleted.
## Conclusion: same distribution does not mean the variables should be removed...
