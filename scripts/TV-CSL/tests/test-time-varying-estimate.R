source("R/data-reader.R")
source("scripts/TV-CSL/time-varying-estimate.R")


# Test TV_CSL_nuisance
data <- read_TV_CSL_nuisance_data(k = 1)
fold_nuisance <- data$fold_nuisance
fold_causal <- data$fold_causal
train_data_original_nuisance <- data$train_data_original_nuisance

regressor_spec <- "linear-only"
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

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- test_data$CATE

MSE <- mean((CATE_true - CATE_est)^2)
## Issue now: fit_TV_CSL_ret$beta_CATE is quite bad


######
#### Test Lasso
n = 500; i <- 1
eta_type <- "10-dim-non-linear"
CATE_type <- "linear"
train_data_origin <- 
  read_single_simulation_data(
    n = n, 
    i = i, 
    eta_type = eta_type,
    CATE_type = CATE_type)$data

train_data <- 
  preprocess_data(single_data = train_data_origin, 
                  run_time_varying = T)
test_data <- 
  read_single_simulation_data(
    n = n, 
    i = i + 100, 
    eta_type = eta_type,
    CATE_type = CATE_type)$data


regressor_spec <- "linear-only"
CATE_spec <- "correctly-specified"
source("scripts/TV-CSL/time-varying-estimate.R")

## Test S_lasso <- function(train_data, test_data, regressor_spec, CATE_spec)
lasso_ret <-
  S_lasso(train_data = train_data,
          test_data = test_data,
          regressor_spec = regressor_spec,
          CATE_spec = CATE_spec)


