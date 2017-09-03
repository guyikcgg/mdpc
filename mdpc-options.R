mdpc.options = list()


# Options for DATA LOAD
mdpc.options$data.load = list(
  # Fold for test set (the other ones will be used as training set)
  k = 5
)


# Options for PREPROCESSING
mdpc.options$preprocessing = list(
  # Ouliers removal (univariate)
  outliersUNI = T,
  
  # Ouliers removal (multivariate)
  outliersMCD = T,
  
  # Discretization
  discretization = NA,
  
  # Attribute selection (correlation cutoff for redundant attributes)
  attribute.selection = 1.0,
  
  # Noise filtering (select either "ENN" or "IPF")
  noise.filter = "IPF"
)


# Options for CLASSIFICATION
mdpc.options$classification = list(
  # Method (select a method, such as rpart, rf of svmLinear)
  method = "rpart",
  
  # Number of cores to use in paralelization
  cores = 3,
  
  # Cross validation options
  cv = list(
    number = 10,
    repeats = 2
  ),
  
  # Input downsampled data
  down = T,
  
  # Input upsampled data
  up = NA,
  
  # Input upsampled data (using SMOTE)
  smote = NA
)

