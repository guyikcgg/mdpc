################################################################
#      Mineria de Datos: Preprocesamiento y Clasificacion      #
#                                                              #
# FILE: classification-down.R                                  #
#                                                              #
# (C) Cristian González Guerrero                               #
################################################################


# Use the same seed to ensure same cross-validation splits
ctrl$seeds = fit$original$control$seeds


# Build down-sampled model
ctrl$sampling = "down"

fit$down = caret::train(
  class ~ .,
  data = training.set,
  method = class.opts$method,
  # verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

print(
  fit$down %>%
    test_roc(data = test.set) %>%
    auc()
)


end.time = Sys.time()
save.image(paste0(class.opts$method, "-down",  ".RData"))
