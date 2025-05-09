
# Test TV_CSL for m_regression
data <- read_TV_CSL_nuisance_data(
  n = 500,
  eta_type = "linear",
  HTE_type = "linear")
fold_nuisance <- data$fold_nuisance
fold_causal <- data$fold_causal
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_HTE
HTE_est <- fit_TV_CSL_ret$HTE_est
HTE_true <- train_data_original_nuisance$HTE

MSE <- mean((HTE_true - HTE_est)^2)
print("MSE")
print(MSE)


data <- read_TV_CSL_nuisance_data(
  n = 500,
  eta_type = "linear",
  HTE_type = "linear")
fold_nuisance <- data$fold_nuisance
# fold_causal <- data$fold_causal
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  load_or_generate_test_data_m_regression(
    n = 500,
    lambda_C = 0.1,
    eta_type = "linear",
    HTE_type = "linear",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 58
  )$train_data
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_HTE
HTE_est <- fit_TV_CSL_ret$HTE_est
HTE_true <- train_data_original_nuisance$HTE

MSE <- mean((HTE_true - HTE_est)^2)
print("MSE")
print(MSE)



data <- read_TV_CSL_nuisance_data(
  n = 1000,
  eta_type = "linear",
  HTE_type = "linear")
fold_nuisance <- data$fold_nuisance
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  load_or_generate_test_data_m_regression(
    n = 1000,
    lambda_C = 0.1,
    eta_type = "linear",
    HTE_type = "linear",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 58
  )$train_data
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_HTE
HTE_est <- fit_TV_CSL_ret$HTE_est
HTE_true <- train_data_original_nuisance$HTE

MSE <- mean((HTE_true - HTE_est)^2)
print("MSE")
print(MSE)




# Test TV_CSL
final_model_method <- "coxph"

TV_CSL_ret <- TV_CSL(train_data = , 
                     test_data = test_data,  
                     train_data_original = train_data_original_nuisance, 
                     folds, 
                     K, 
                     prop_score_spec, 
                     lasso_type, 
                     regressor_spec, 
                     final_model_method) 

# Test TV_CSL_nuisance
data <- read_TV_CSL_nuisance_data(k = 1)
fold_nuisance <- data$fold_nuisance
fold_causal <- data$fold_causal
train_data_original_nuisance <- data$train_data_original_nuisance

regressor_spec <- "linear"
prop_score_spec <- "cox-linear-all-data"
lasso_type <- "S-lasso"
source("scripts/TV-CSL/time-varying-estimate.R")
fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = prop_score_spec,
  lasso_type = lasso_type,
  regressor_spec = regressor_spec
)
source("scripts/TV-CSL/time-varying-estimate.R")
final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = test_data
)

fit_TV_CSL_ret$beta_HTE
HTE_est <- fit_TV_CSL_ret$HTE_est
HTE_true <- test_data$HTE

MSE <- mean((HTE_true - HTE_est)^2)
## Issue now: fit_TV_CSL_ret$beta_HTE is quite bad
