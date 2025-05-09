source("R/data-handler.R")
source("scripts/TV-CSL/time-varying-estimate.R")
source("scripts/TV-CSL/tests/test-data-handler.R")
source("scripts/TV-CSL/tests/test-helper.R")
library(here)
library(testthat)
library(survival)

## Test transform_X
test_that("linear transformation returns correct number of columns", {
  # Sample data for testing
  set.seed(123)
  df_sample <- data.frame(
    X.1 = rnorm(100, mean = 50, sd = 10),   # Continuous variable (age)
    X.2 = rbinom(100, 1, 0.5),              # Binary variable (surgery)
    X.3 = rnorm(100, mean = 5, sd = 2),     # Continuous variable (year)
    W = rbinom(100, 1, 0.5)                 # Treatment indicator
  )
  
  transformed <- transform_X(df_sample, regressor_spec = "linear")
  
  expect_equal(ncol(transformed), 3)
  expect_equal(colnames(transformed), c("X.1", "X.2", "X.3"))
})

test_that("complex transformation includes splines, square, and interaction terms", {
  # Sample data for testing
  set.seed(123)
  df_sample <- data.frame(
    X.1 = rnorm(100, mean = 50, sd = 10),   # Continuous variable (age)
    X.2 = rbinom(100, 1, 0.5),              # Binary variable (surgery)
    X.3 = rnorm(100, mean = 5, sd = 2),     # Continuous variable (year)
    W = rbinom(100, 1, 0.5)                 # Treatment indicator
  )
  
  transformed <- transform_X(df_sample, regressor_spec = "complex")
  
  expect_true(any(grepl("X.1_spline", colnames(transformed))))
  expect_true(any(grepl("X.3_spline", colnames(transformed))))
  
  expect_true(any(grepl("X.1_squared", colnames(transformed))))
  expect_true(any(grepl("X.3_squared", colnames(transformed))))
  
  expect_true(any(grepl("X.1_x_X.2", colnames(transformed))))
  expect_true(any(grepl("X.1_x_X.3", colnames(transformed))))
  expect_true(any(grepl("X.2_x_X.3", colnames(transformed))))
})



######
#### Test Lasso
######

test_that("Check correct file paths for lasso", {
  csv_file <- generate_output_path(
    results_dir = "scripts/TV-CSL/results/",
    is_running_cox = FALSE,
    is_running_lasso = TRUE,
    is_running_TV_CSL = FALSE,
    eta_type = "linear",
    HTE_type = "linear",
    n = 500,
    i = 1,
    seed_value = 12345
  )
  
  expected_path <- "scripts/TV-CSL/results/lasso_eta-linear_HTE-linear_n-500/result-iteration_1-seed_12345.csv"
  expect_equal(csv_file, expected_path)
})


test_that("Lasso file is saved correctly", {
  result_df <- data.frame(a = rnorm(5), b = runif(5))
  
  # Set the parameters for lasso
  is_running_cox <- FALSE
  is_running_lasso <- TRUE
  is_running_TV_CSL <- FALSE
  eta_type <- "linear"
  HTE_type <- "linear"
  n <- 500
  i <- 1
  seed_value <- 12345
  
  # Generate the expected CSV file path using the function
  result_csv_file <- generate_output_path(
    is_running_cox = is_running_cox,
    is_running_lasso = is_running_lasso,
    is_running_TV_CSL = is_running_TV_CSL,
    eta_type = eta_type,
    HTE_type = HTE_type,
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


create_dummy_data <- function(n = 100) {
  data.frame(
    tstart = rep(0, n),
    tstop = runif(n, 1, 10),
    Delta = rbinom(n, 1, 0.7),
    W = rbinom(n, 1, 0.5),
    X.1 = rnorm(n),
    X.2 = rnorm(n),
    X.3 = rnorm(n),
    X.4 = rnorm(n),
    HTE = rnorm(n)
  )
}

test_that("S_lasso handles basic inputs correctly", {
  # Setup
  set.seed(123)
  train_data <- create_dummy_data(100)
  test_data <- create_dummy_data(50)
  
  result <- S_lasso(
    train_data = train_data,
    test_data = test_data,
    regressor_spec = "linear",
    HTE_spec = "linear"
  )
  
  # Check output structure
  expect_type(result, "list")
  expect_s3_class(result, "slasso")
  expect_named(result, c("m", "m_beta", "beta_HTE", "beta_eta_0", 
                         "y_0_pred", "y_1_pred", "HTE_est", "HTE_true", "MSE"))
  
  # Check dimensions
  expect_equal(length(result$HTE_est), nrow(test_data))
  expect_equal(length(result$y_0_pred), nrow(test_data))
  expect_equal(length(result$y_1_pred), nrow(test_data))
})

test_that("S_lasso handles different regressor specifications", {
  set.seed(123)
  train_data <- create_dummy_data(100)
  test_data <- create_dummy_data(50)

  result <- suppressWarnings(
    S_lasso(
      train_data = train_data,
      test_data = test_data,
      regressor_spec = "complex",
      HTE_spec = "linear"
    )
  )
  
  # Check output structure
  expect_type(result, "list")
  expect_s3_class(result, "slasso")
  expect_named(result, c("m", "m_beta", "beta_HTE", "beta_eta_0", 
                         "y_0_pred", "y_1_pred", "HTE_est", "HTE_true", "MSE"))
  
  # Check dimensions
  expect_equal(length(result$HTE_est), nrow(test_data))
  expect_equal(length(result$y_0_pred), nrow(test_data))
  expect_equal(length(result$y_1_pred), nrow(test_data))
})



test_that("m_regression function returns numeric m_beta", {
  
  data <- read_TV_CSL_nuisance_data(k = 1)
  fold_nuisance <- data$fold_nuisance
  fold_causal <- data$fold_causal
  train_data_original_nuisance <- data$train_data_original_nuisance
  
  # Run m_regression
  regressor_spec <- "linear"
  m_ret <- m_regression(
    train_data = fold_nuisance, 
    test_data = fold_causal, 
    regressor_spec = regressor_spec,
    verbose = 0
  )
  
  # Test that m_ret$m_beta is numeric
  expect_type(as.numeric(m_ret$m_beta), "double")
})



test_that("m_regression recovers true coefficients with zero HTE and linear baseline", {
  slope_multiplier <- 2.5
  true_coefficients <- slope_multiplier * 1/(1:10)
  
  source("scripts/TV-CSL/tests/test-helper.R")
  test_data <- load_or_generate_test_data_m_regression()
  train_data_pseudo <- test_data$train_data_pseudo
  
  
  m_ret <- m_regression(
    train_data = train_data_pseudo, 
    test_data = train_data_pseudo, 
    regressor_spec = "linear",
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






