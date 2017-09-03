################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: classification-smote.R                                 #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################


# Use the same seed to ensure same cross-validation splits
ctrl$seeds = fit$original$control$seeds


# Build up-sampled model using SMOTE
ctrl$sampling = "smote"

fit$smote = caret::train(
  class ~ .,
  data = training.set,
  method = class.opts$method,
  # verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

print(
  fit$smote %>%
    test_roc(data = test.set) %>%
    auc()
)


end.time = Sys.time()
save.image(paste0(class.opts$method, "-smote",  ".RData"))
