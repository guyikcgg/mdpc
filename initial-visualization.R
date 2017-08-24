
# Load required libraries
library(utils)
library(ggplot2)
library(reshape)
library(GGally)

# Load the dataset
data = read.csv("data/datcom2016.csv")

# Set the random seed in order to be able to reproduce the results
set.seed(1)


# Get 10% of the data (already many points)
data.sample = data[sample(1:(0.01*nrow(data))), ]


# Visualize the structure of the data
str(data)
summary(data)


# Plot frequency of each class
ggplot(data) +
  geom_bar(aes(class, fill = class))

## We are facing an unbalanced problem


# Check for missing values
as.data.frame(colSums(is.na(data)))

## There are no missing values


# Separate data in numeric and factor
data.numeric = data[,sapply(data, class) != "factor"]
data.factor  = data[,sapply(data, class) == "factor"]
data.sample.numeric = data.sample[,sapply(data, class) != "factor"]
data.sample.factor  = data.sample[,sapply(data, class) == "factor"]

# Plot numeric data
## Distributions
myData = melt.data.frame(
  cbind(data.numeric, class = data$class)
)
ggplot(myData) +
  geom_density(aes(value)) +
  geom_density(aes(value, color = class)) +
  facet_wrap(~variable, scales = "free")

ggplot(myData) +
  geom_freqpoly(aes(value)) +
  geom_freqpoly(aes(value, color = class)) +
  facet_wrap(~variable, scales = "free")



# Check for similar distributions
ks.test.results = matrix(
  nrow = ncol(data.numeric), 
  ncol = ncol(data.numeric)
)
for (i in 1:(ncol(data.numeric)-1)) {
  for (j in (i+1):ncol(data.numeric)) {
    ks.test.results[i,j] = ks.test(data.numeric[,i], data.numeric[,j])$p.value
  }
}


sum(ks.test.results>0.20, na.rm = T)
## There are 3 variables that can be removed without any other consideration

sum(ks.test.results>0.05, na.rm = T)
## There is 1 additional variable that could surely be removed

sum(ks.test.results>0.01, na.rm = T)
## There are 2 additional variable that might be removed


## Since there are many variables and KS test is very restrictive, 
## we are going to remove the 6 variables. But the correlations
## will be checked before.

correlations = cor(data.numeric)
(sum(abs(correlations) > 0.8) - 41)/2



which(ks.test.results>0.20, arr.ind = T)
which((abs(correlations) > 0.8) & (abs(correlations) < 1), arr.ind = T)

data.sample.correlated = data.sample.numeric[,which((abs(correlations) > 0.8) & (abs(correlations) < 1), arr.ind = T)[,1]]
# myData = melt(data.sample.correlated)
# ggplot(data.sample.correlated) +
#   geom_point(aes(values, value)) +
#   facet_grid(.~variable)

ggpairs(data.sample.correlated, aes(alpha = (10/nrow(data.sample))))


data.sample.same_distro = data.sample.numeric[,which(ks.test.results > 0.01, arr.ind = T)[,1]]
ggpairs(data.sample.same_distro, aes(alpha = (10/nrow(data.sample))))

ggplot(data) + geom_jitter(aes(PredSS_r1_.1, PredSS_r1, color = class), alpha = 0.1)

myData = subset(data, select = c(PredCN_r1, PredCN_r2, class))
myData = melt(myData)
ggplot(myData) +
  geom_density(aes(value)) +
  geom_density(aes(value, color = class)) +
  facet_wrap(~variable, ncol = 1)
ggplot(data) + geom_jitter(aes(PredCN_r1, PredCN_r1, color = class), alpha = 0.1)
ggplot(data) + geom_jitter(aes(PredCN_r1, PredCN_r2, color = class), alpha = 0.1)
ggplot(data) + geom_bar(aes(class, fill = as.factor(PredCN_r1)))
ggplot(data) + geom_bar(aes(class, fill = as.factor(PredCN_r2)))
cor(data$PredCN_r1, data$PredCN_r2)
ks.test(data$PredCN_r1, data$PredCN_r2)
ks.test(
  subset(data, subset = class == "positive")$PredCN_r1, 
  subset(data, subset = class == "positive")$PredCN_r2
)
ks.test(
  subset(data, subset = class == "negative")$PredCN_r1, 
  subset(data, subset = class == "negative")$PredCN_r2
)
# They are not correlated, but they hold the same information. 
# This might induce to think that we could omit one of the variables.
# However, having the two variables makes a great decission boundary,
# so none of them should be deleted.

# Conclusion -> Same distribution does not mean the variables should be removed...



# Check for similar distributions btween positive and negative
ks.test.results.pn = matrix(
  nrow = ncol(data.numeric), 
  ncol = 1
)
rownames(ks.test.results.pn) = names(data.numeric)
myData.p = subset(cbind(data.numeric, class = data$class), subset = class == "positive")
myData.n = subset(cbind(data.numeric, class = data$class), subset = class == "negative")
for (i in 1:(ncol(data.numeric))) {
    ks.test.results.pn[i,1] = ks.test(
      myData.p[,i], 
      myData.n[,i]
    )$p.value
}

rownames(ks.test.results.pn)[ks.test.results.pn>0.1]

## Having the same distribution for positive and negative is not very informative...