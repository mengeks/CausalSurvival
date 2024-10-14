source("R/data-reader.R")
source("scripts/TV-CSL/time-varying-estimate.R")


# Test TV_CSL
TV_CSL_ret <- TV_CSL(train_data, 
                     test_data, 
                     train_data_original, 
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





## Test transform_X

library(testthat)

# Sample data for testing
set.seed(123)
df_sample <- data.frame(
  X.1 = rnorm(100, mean = 50, sd = 10),   # Continuous variable (age)
  X.2 = rbinom(100, 1, 0.5),              # Binary variable (surgery)
  X.3 = rnorm(100, mean = 5, sd = 2),     # Continuous variable (year)
  W = rbinom(100, 1, 0.5)                 # Treatment indicator
)
source("scripts/TV-CSL/time-varying-estimate.R")

# Tests for transform_X

test_that("linear-only transformation returns correct number of columns", {
  transformed <- transform_X(df_sample, regressor_spec = "linear-only")
  
  expect_equal(ncol(transformed), 3)
  expect_equal(colnames(transformed), c("X.1", "X.2", "X.3"))
})

test_that("mild-complex transformation includes splines, square, and interaction terms", {
  transformed <- transform_X(df_sample, regressor_spec = "mild-complex")
  
  expect_true(any(grepl("X.1_spline", colnames(transformed))))
  expect_true(any(grepl("X.3_spline", colnames(transformed))))
  
  expect_true(any(grepl("X.1_squared", colnames(transformed))))
  expect_true(any(grepl("X.3_squared", colnames(transformed))))
  
  expect_true(any(grepl("X.1_x_X.2", colnames(transformed))))
  expect_true(any(grepl("X.1_x_X.3", colnames(transformed))))
  expect_true(any(grepl("X.2_x_X.3", colnames(transformed))))
})

