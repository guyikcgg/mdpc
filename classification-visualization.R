################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: classification-visualization.R                         #
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

# Visualize the results
## Calculate ROC for every element in the `fit` list
model_list_roc = fit %>%
  map(test_roc, data = test.set)

## Calculate the AUC
print(
  model_list_roc %>%
    map(function(x) return(auc(x)))
)

# Get a table to put in the document
test_measures = data.frame(AUC = as.numeric(lapply(model_list_roc, function(x) return(auc(x)))))
test_measures$Accuracy = as.numeric(lapply(fit, test_accuracy, data = test.set))
test_measures$Kappa = as.numeric(lapply(fit, test_kappa, data = test.set))
rownames(test_measures) = names(fit)

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
  
  if (class.opts$method == "rpart") {
    print(fancyRpartPlot(fit[[i]]$finalModel, sub = names(fit)[i]))
  }
  
  i = i + 1
}

results_df_roc = bind_rows(results_list_roc)


# Plot ROC curve for all tested models
print(
  ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
    geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
    geom_line(aes(color = model), size = 1)
)

