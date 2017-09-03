################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: classification-orig.R                                  #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################

# Load required libraries
library(dplyr)
library(caret)
library(DMwR)
library(purrr)
library(pROC)

# Beautiful tree graphics
if (class.opts$method == "rpart") {
  library(rattle)  
}

# Paralelization
if (!is.na(class.opts$cores)) {
  library(doMC)
  registerDoMC(cores = class.opts$cores)
}


start.time = Sys.time()


# FUNCTIONS
test_roc = function(model, data) {
  # Calculate the ROC for the minoritary class with test data
  roc(
    data$class,
    predict(model, data, type = "prob")[, "positive"]
  )
}

test_accuracy = function(model, data) {
  return(confusionMatrix(predict(model, data), data$class)$overall["Accuracy"])
}

test_kappa = function(model, data) {
  return(confusionMatrix(predict(model, data), data$class)$overall["Kappa"])
}



# Setup control function for training
ctrl = trainControl(
  method = "repeatedcv",
  number = class.opts$cv$number,
  repeats = class.opts$cv$repeats,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  classProbs = TRUE
)


# Build a list to store the results
fit = list()

# Set the random seed
set.seed(1234)

# Learn a model with the original data
fit$original = caret::train(
  class ~ .,
  data = training.set,
  method = class.opts$method,
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

end.time = Sys.time()
save.image(paste0(class.opts$method, "-orig", ".RData"))
