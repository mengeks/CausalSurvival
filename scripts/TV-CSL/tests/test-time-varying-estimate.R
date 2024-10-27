source("R/data-handler.R")
source("scripts/TV-CSL/time-varying-estimate.R")
library(here)
library(testthat)
library(survival)

test_that("m_regression function returns numeric m_beta", {
  
  data <- read_TV_CSL_nuisance_data(k = 1)
  fold_nuisance <- data$fold_nuisance
  fold_causal <- data$fold_causal
  train_data_original_nuisance <- data$train_data_original_nuisance
  
  # Run m_regression
  regressor_spec <- "linear-only"
  m_ret <- m_regression(
    train_data = fold_nuisance, 
    test_data = fold_causal, 
    regressor_spec = regressor_spec,
    verbose = 0
  )
  
  # Test that m_ret$m_beta is numeric
  expect_type(as.numeric(m_ret$m_beta), "double")
})



test_that("m_regression recovers true coefficients with zero CATE and linear baseline", {
  slope_multiplier <- 2.5
  true_coefficients <- slope_multiplier * 1/(1:10)
  
  source("scripts/TV-CSL/tests/test-helper.R")
  test_data <- load_or_generate_test_data_m_regression()
  train_data_pseudo <- test_data$train_data_pseudo
  
  
  m_ret <- m_regression(
    train_data = train_data_pseudo, 
    test_data = train_data_pseudo, 
    regressor_spec = "linear-only",
    verbose = 0
  )
  
  
  mre_results <- calculate_min_relative_error(
    true_coef = true_coefficients,
    est_coef = m_ret$m_beta
  )
  
  
  expect_lt(mre_results$min_relative_error, 0.01,
            label = "Minimum relative error should be less than 1%")
  
  
  expect_equal(length(m_ret$m_beta), length(true_coefficients),
               label = "Number of coefficients should match")
  expect_true(all(!is.na(m_ret$m_beta)),
              label = "No coefficients should be NA")
  
})

test_transformed_X <- transform_X(
  single_data = fold_causal,
  regressor_spec = regressor_spec)
y_pred_expect <- as.vector(test_transformed_X %*% m_ret$m_beta)
all.equal(y_pred_expect, m_ret$y_pred) 

m_ret <- m_regression(train_data = fold_nuisance, 
                      regressor_spec = "mild-complex",
                      test_data = fold_causal, 
                      verbose = 0)
m_ret$m_beta

# Test TV_CSL for m_regression
fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)
source("scripts/TV-CSL/time-varying-estimate.R")
final_model_method <- "lasso"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = test_data
)

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- test_data$CATE

MSE <- mean((CATE_true - CATE_est)^2)


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



library(testthat)


test_that("Check correct file paths for lasso", {
  csv_file <- generate_output_path(
    is_running_cox = FALSE,
    is_running_lasso = TRUE,
    is_running_TV_CSL = FALSE,
    eta_type = "10-dim-linear",
    CATE_type = "ReLU",
    n = 500,
    i = 1,
    seed_value = 12345
  )
  
  expected_path <- "scripts/TV-CSL/results/lasso_eta-10-dim-linear_CATE-ReLU_n-500/lasso_eta-10-dim-linear_CATE-ReLU_n-500-result-iteration_1-seed_12345.csv"
  expect_equal(csv_file, expected_path)
})

test_that("Lasso file is saved correctly", {
  # Create a mock result dataframe
  result_df <- data.frame(a = rnorm(5), b = runif(5))
  
  # Set the parameters for lasso
  is_running_cox <- FALSE
  is_running_lasso <- TRUE
  is_running_TV_CSL <- FALSE
  eta_type <- "10-dim-linear"
  CATE_type <- "linear"
  n <- 500
  i <- 1
  seed_value <- 12345
  
  # Generate the expected CSV file path using the function
  result_csv_file <- generate_output_path(
    is_running_cox = is_running_cox,
    is_running_lasso = is_running_lasso,
    is_running_TV_CSL = is_running_TV_CSL,
    eta_type = eta_type,
    CATE_type = CATE_type,
    n = n,
    i = i,
    seed_value = seed_value
  )
  
  # Write the CSV file using the generated path
  write.csv(result_df, result_csv_file, row.names = FALSE)
  
  
  loaded_df <- read.csv(result_csv_file)
  expect_equal(loaded_df, result_df)
  
  # Cleanup: remove the saved file after test
  unlink(result_csv_file)
})
