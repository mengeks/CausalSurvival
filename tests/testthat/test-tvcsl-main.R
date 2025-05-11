########
# TV-CSL testthat Tests
########

library(testthat)
library(survival)
library(dplyr)

# Source the refactored TV-CSL package
source("R/tvcsl.R")

# Create a simple test dataset
create_test_data <- function(n = 100) {
  set.seed(123)
  
  # Generate covariates
  X1 <- rnorm(n)
  X2 <- rbinom(n, 1, 0.5)
  
  # Generate treatment time
  tx_time <- rexp(n, rate = 0.1) + 2 * X1 + X2
  tx_time[tx_time < 0] <- 0
  
  # Make some subjects untreated
  untreated <- sample(1:n, n/5)
  tx_time[untreated] <- Inf
  
  # Generate event time
  lambda0 <- 0.1
  beta <- c(0.5, -0.3)
  alpha <- 1.5  # Treatment effect
  
  # Generate control survival time
  control_time <- rexp(n, rate = lambda0 * exp(X1 * beta[1] + X2 * beta[2]))
  
  # Generate treated survival time
  treated_time <- rexp(n, rate = lambda0 * exp(X1 * beta[1] + X2 * beta[2] + alpha))
  
  # Combine to get observed time
  true_time <- ifelse(tx_time < Inf, 
                      pmin(tx_time, control_time) + 
                        ifelse(tx_time < control_time, 
                               treated_time, 0),
                      control_time)
  
  # Generate censoring time
  cens_time <- rexp(n, rate = 0.05)
  
  # Combine to get observed time and status
  obs_time <- pmin(true_time, cens_time)
  status <- as.integer(true_time <= cens_time)
  
  # Create data frame
  data <- data.frame(
    id = 1:n,
    X1 = X1,
    X2 = X2,
    tx_time = tx_time,
    obs_time = obs_time,
    status = status,
    true_effect = alpha + 0*X1  # Constant treatment effect
  )
  
  return(data)
}

# Test dataset
test_data <- create_test_data(100)

# Create time-varying dataset
test_data_tv <- create_time_varying_dataset(
  data = test_data,
  event_time = "obs_time",
  event_indicator = "status",
  treatment_time = "tx_time",
  covariates = c("X1", "X2"),
  id = "id"
)

# Test suite
test_that("create_time_varying_dataset works correctly", {
  # Check dimensions
  expect_true(nrow(test_data_tv) >= nrow(test_data))
  
  # Check column names
  expect_true(all(c("tstart", "tstop", "event_indicator", "treatment_indicator") %in% colnames(test_data_tv)))
  
  # Check that untreated subjects have treatment_indicator = 0
  untreated_ids <- test_data$id[is.infinite(test_data$tx_time)]
  expect_true(all(test_data_tv$treatment_indicator[test_data_tv$id %in% untreated_ids] == 0))
})

test_that("quick_tvcsl works correctly", {
  # Fit model
  model <- quick_tvcsl(
    data = test_data_tv,
    event_time = "tstop",
    event_indicator = "event_indicator",
    treatment_time = "tx_time",
    covariates = c("X1", "X2"),
    id = "id",
    treatment_effect_form = "constant"
  )
  
  # Check that model has expected components
  expect_true("coefficients" %in% names(model))
  expect_true("tvcsl" %in% class(model))
  
  # Check coefficients
  expect_equal(length(model$coefficients), 1)  # Constant effect has one coefficient
})

test_that("tvcsl with linear treatment effects works correctly", {
  # Fit model
  model <- tvcsl(
    formula = Surv(tstart, tstop, event_indicator) ~ X1 + X2,
    data = test_data_tv,
    treatment_time = "tx_time",
    treatment_effect_form = "linear",
    baseline_form = "linear",
    propensity_model = "cox-linear",
    outcome_model = "s-learner",
    final_model_method = "coxph",
    cv_folds = 2,  # Use fewer folds for testing
    id = "id",
    fast_lasso = TRUE
  )
  
  # Check that model has expected components
  expect_true("coefficients" %in% names(model))
  expect_true("tvcsl" %in% class(model))
  
  # Check coefficients
  expect_equal(length(model$coefficients), 3)  # Intercept + 2 covariates
})

test_that("predict.tvcsl works correctly", {
  # Fit model
  model <- quick_tvcsl(
    data = test_data_tv,
    event_time = "tstop",
    event_indicator = "event_indicator",
    treatment_time = "tx_time",
    covariates = c("X1", "X2"),
    id = "id",
    treatment_effect_form = "constant"
  )
  
  # Make predictions
  preds <- predict(model, test_data)
  
  # Check predictions
  expect_equal(length(preds), nrow(test_data))
  expect_true(is.numeric(preds))
})

test_that("evaluate_tvcsl works correctly", {
  # Fit model
  model <- quick_tvcsl(
    data = test_data_tv,
    event_time = "tstop",
    event_indicator = "event_indicator",
    treatment_time = "tx_time",
    covariates = c("X1", "X2"),
    id = "id",
    treatment_effect_form = "constant"
  )
  
  # Evaluate model
  eval_results <- evaluate_tvcsl(model, test_data, "true_effect")
  
  # Check results
  expect_true("mse" %in% names(eval_results))
  expect_true("mae" %in% names(eval_results))
  expect_true(is.numeric(eval_results$mse))
  expect_true(is.numeric(eval_results$mae))
})