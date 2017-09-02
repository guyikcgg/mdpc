# Clear workspace
closeAllConnections()
rm(list=ls())

library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculation
library(rattle)
# library(doMC)
# registerDoMC(cores = 2)


# Build custom AUC function to extract AUC
# from the caret model object
test_roc = function(model, data) {
  # Calculate the ROC for the minoritary class
  roc(
    data$class,
    predict(model, data, type = "prob")[, "positive"]
  )
}

rm.variables = F
plot.enable = F


# Load data
source("load-data.R")

# Change the default training and test sets
k = 2

dataset.tra = dataset.train(k)
dataset.tra.cl = dataset.tra$class
dataset.tra.dt = subset(dataset.tra, select = -class)
dataset.tst = dataset.test(k)
dataset.tst.cl = dataset.tst$class
dataset.tst.dt = subset(dataset.tst, select = -class)

rm(dataset)

# Preprocess the training dataset
# source("outlier-removal.R")
# source("discretization.R")
if (!exists("dataset.tra.preprocessed")) {
  dataset.tra.preprocessed = dataset.tra
  dataset.tra.preprocessed.dt = dataset.tra.dt
  dataset.tra.preprocessed.cl = dataset.tra.cl
}
# source("attribute-selection.R")
# source("noise-filter.R")





# Set the input to tree-based classification
training.set = dataset.tra #IPF.out$cleanData
test.set  = dataset.tst

# Print some statistics about the training set and test set
print(prop.table(table(training.set$class)))
print(prop.table(table(test.set$class)))


# Setup control function for training
ctrl = trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  classProbs = TRUE
)


# Build a standard classifier using a gradient boosted machine (or not!)

set.seed(1234)

fit = list()


fit$original = caret::train(
  class ~ .,
  data = training.set,
  method = "rpart",
  metric = "ROC",
  trControl = ctrl
)

print(
  fit$original %>%
    test_roc(data = test.set) %>%
    auc()
)


# Use the same seed to ensure same cross-validation splits
ctrl$seeds = fit$original$control$seeds


# Build down-sampled model
ctrl$sampling = "down"

fit$down = caret::train(
  class ~ .,
  data = training.set,
  method = "rpart",
  # verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

print(
  fit$down %>%
    test_roc(data = test.set) %>%
    auc()
)


# Build up-sampled model
ctrl$sampling = "up"

fit$up = caret::train(
  class ~ .,
  data = training.set,
  method = "rpart",
  # verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

print(
  fit$up %>%
    test_roc(data = test.set) %>%
    auc()
)


# # Build smote model
# ctrl$sampling = "smote"
# 
# fit$SMOTE = caret::train(
#   class ~ .,
#   data = training.set,
#   method = "rpart",
#   # verbose = FALSE,
#   metric = "ROC",
#   trControl = ctrl
# )
# 
# print(
#   fit$SMOTE %>%
#     test_roc(data = test.set) %>%
#     auc()
# )


# Examine the results
## Calculate ROC for every element in the `fit` list
model_list_roc = fit %>%
  map(test_roc, data = test.set)

## Calculate the AUC
print(
  model_list_roc %>%
    map(function(x) return(auc(x)))
)

# PLOT
# Build a dataframe to plot
results_list_roc = list()
i = 1

for(the_roc in model_list_roc){
  results_list_roc[[i]] = 
    data_frame(
      tpr = the_roc$sensitivities,
      fpr = 1 - the_roc$specificities,
      model = names(fit)[i]
    )
  
  i = i + 1
}

results_df_roc = bind_rows(results_list_roc)

# Plot ROC curve for all tested models
ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  geom_line(aes(color = model), size = 1)

# fancyRpartPlot(fit$up$finalModel, sub="")