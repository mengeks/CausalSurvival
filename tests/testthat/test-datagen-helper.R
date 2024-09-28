library(tidyverse)
library(survival)
library(here)
source("R/datagen-helper.R")
library(testthat)

test_that("generate_simulated_data produces correct structure for non-time-varying data", {
  n <- 200
  CATE_type <- "non-linear"
  eta_type <- "10-dim-non-linear"
  
  other_params <- list(
    lambda_C = 0.1,   
    p = 10,
    X_distribution = "normal", 
    X_cov_type = "toeplitz",
    tx_difficulty = "simple"
  )
  params <- c(list(
    n = n,
    CATE_type = CATE_type,
    eta_type = eta_type,
    seed_value = 123
  ),other_params)
  
  source("R/datagen-helper.R")
  simulated_data_i_n <- generate_simulated_data(
    n, 
    lambda_C = params$lambda_C,
    p = params$p,  
    eta_type = params$eta_type,
    X_distribution = params$X_distribution, 
    X_cov_type = params$X_cov_type,
    tx_difficulty = params$tx_difficulty,
    CATE_type = params$CATE_type,
    seed_value = params$seed_value,
    verbose = 0
  )
  
  # Check if the data is a data.frame
  expect_true(is.data.frame(simulated_data_i_n), "The output should be a data.frame")
  
  # Check if the number of rows is correct
  expect_equal(
    nrow(simulated_data_i_n), n)
  
  expect_true("A" %in% colnames(simulated_data_i_n), "The column 'A' should exist in non-time-varying data")
  
  expect_true(all(c("T", "U", "Delta", "C") %in% colnames(simulated_data_i_n)), 
              "The essential columns 'T', 'U', 'Delta', and 'C' should be present")
})


test_that("sigma function works for normal(0,1) input and plots its behavior", {
  sigma <- function(x) 2 / (1 + exp(-12 * (x - 0.5)))
  # Step 1: Generate 1000 samples from a normal(0,1) distribution
  set.seed(123)  # For reproducibility
  x <- rnorm(1000, mean = 0, sd = 1)
  
  # Step 2: Apply the sigma function to the samples
  sigma_values <- sigma(x)
  hist(sigma_values)
  
  x_vals <- seq(-3, 3, length.out = 100)  # Values from -3 to 3
  y_vals <- sigma(x_vals)
  
  # Create the plot using ggplot2
  plot_data <- data.frame(x = x_vals, sigma = y_vals)
  
  p <- ggplot(plot_data, aes(x = x, y = sigma)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Plot of Sigma Function", x = "x", y = "sigma(x)") +
    theme_minimal()
  
  print(p)
})


test_that("generate_simulated_data generates good data for eta_type  = 10-dim-non-linear", {
  n <- 200
  eta_type <- "10-dim-non-linear"
  lambda_C = 0.1
  seed_value = 123
  
  source("R/datagen-helper.R")
  sim_non_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "non-linear",
    seed_value = params$seed_value,
    verbose = 0
  )
  
  # test: CATE better have a range of values
  hist(sim_non_linear$CATE)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_non_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_non_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the range of non-censored time should not be too small
  hist(sim_non_linear$T)
  expect_true(max(sim_non_linear$T) >= 9)
  
  
  source("R/datagen-helper.R")
  sim_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = seed_value,
    verbose = 0
  )
  # test: the range of non-censored time should not be too small
  hist(sim_linear$T)
  expect_true(max(sim_linear$T) >= 9)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_linear$CATE)
  
  sim_zero <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "zero",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_zero$T)
  expect_true(max(sim_zero$T) >= 9)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_zero %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_zero %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_zero$CATE)
  
  sim_constant <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "constant",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_constant$T)
  expect_true(max(sim_constant$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_constant %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_constant %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_constant$CATE)
  
  sim_zero <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "zero",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_zero$T)
  expect_true(max(sim_zero$T) >= 9)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_zero %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_zero %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_zero$CATE)
  
  sim_ReLU <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "ReLU",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_ReLU$T)
  expect_true(max(sim_ReLU$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_ReLU %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_ReLU %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_ReLU$CATE)
})




# --------------------------
# Test: run_simulation function
# --------------------------

# Define simulation parameters
params <- list(
  light_censoring = TRUE,
  lambda_C = 20,
  tau = 1,
  p = 5,
  beta = rep(1, 5),  # 5 predictors, all with coefficient 1
  delta = 0.5,
  eta_type = "linear-interaction", 
  baseline_type = "cosine"  
)

# Define n and repetition parameters for test
n_list <- c(10)  # Small test with 10 samples
R <- 2  # Number of repetitions
is_time_varying_range <- c(TRUE, FALSE)  # Test both time-varying and non-time-varying

# Run the simulation with test parameters
run_simulation(n_list, R, is_time_varying_range, params)

# --------------------------
# Verify Generated Data
# --------------------------

# Load one generated dataset to verify structure
loaded_data <- readRDS("data/simulated/time-varying/sim_data_n_10_R_2/sim_data_1_seed_134.rds")

# Inspect the structure of the loaded data
str(loaded_data)

# Access the simulated data and parameters
simulated_data <- loaded_data$data
sim_params <- loaded_data$params


# Test generate_simulated_data function
# generate_simulated_data should be tested separately with different parameter settings 
# to ensure it works for both time-varying and non-time-varying scenarios.


test_that("generate_simulated_data produces correct structure for time-varying data", {
  n <- 200
  p <- 5
  
  # Call the generate_simulated_data function for time-varying case
  data <- generate_simulated_data(n = n, 
                                  is_time_varying = TRUE, 
                                  light_censoring = TRUE, 
                                  lambda_C = 0.1, 
                                  tau = 1, 
                                  p = p, 
                                  beta = rep(1, p), 
                                  delta = 0.5)
  
  # Check if the data is a data.frame
  expect_true(is.data.frame(data), "The output should be a data.frame")
  
  # Check if the number of rows is correct
  expect_equal(nrow(data), n)
  
  # Check if columns like A exist for time-varying data
  expect_true("A" %in% colnames(data), "The column 'A' should exist in time-varying data")
  expect_false("W" %in% colnames(data), "The column 'W' should not exist in time-varying data")
  
  # Check for the presence of essential columns
  expect_true(all(c("T", "U", "Delta", "C") %in% colnames(data)), 
              "The essential columns 'T', 'U', 'Delta', and 'C' should be present")
})


test_that("calculate_hazard calculates non-time-varying hazard correctly", {
  # Set up test inputs
  t <- 5  # Time point
  x <- data.frame(
    "X.1" = c(0.5, -0.3), 
    "X.2" = c(1.0, -1.5), 
    "X.3" = c(-0.5, 0.3), 
    "X.4" = c(0.2, -0.2), 
    "X.5" = c(-0.1, 0.5),
    "W" = c(1, 0)  # Non-time-varying W
  )
  
  betas <- c(beta1 = 0.5, beta2 = 1, beta3 = -0.5, beta4 = 0.3, beta5 = 0.2, delta = 0.1, tau = 1)
  
  # Call the function
  hazard <- calculate_hazard(t, x, betas, is_time_varying = FALSE)
  
  # Expected values (manually computed)
  eta_0_1 <- (0.5 * 0.5 + 1.0 * 1 + (-0.5) * (-0.5) + 0.2 * 0.3 + (-0.1) * 0.2 + 0.1 * 0.5 * 1.0)
  eta_0_2 <- (-0.3 * 0.5 + (-1.5) * 1 + 0.3 * (-0.5) + (-0.2) * 0.3 + 0.5 * 0.2 + 0.1 * (-0.3) * (-1.5))
  
  expected_hazard_1 <- exp(eta_0_1 + 1 * 1) * (cos(5 * 3) + 1) / 2
  expected_hazard_2 <- exp(eta_0_2 + 0 * 1) * (cos(5 * 3) + 1) / 2
  
  # Test that the calculated hazard matches expected values
  expect_equal(hazard[1], expected_hazard_1, tolerance = 1e-6)
  expect_equal(hazard[2], expected_hazard_2, tolerance = 1e-6)
})

library(testthat)

test_that("calculate_hazard computes correctly for constant hazard baseline", {
  # Set up test inputs
  t <- 5  # Time point (should not affect constant baseline)
  x <- data.frame(
    "X.1" = 0.5, 
    "X.2" = 1.0, 
    "X.3" = -0.5, 
    "X.4" = 0.2, 
    "X.5" = -0.1, 
    "W" = 1  # Non-time-varying W
  )
  
  # Set beta coefficients
  betas <- c(beta1 = 0.5, beta2 = 1, beta3 = -0.5, beta4 = 0.3, beta5 = 0.2, delta = 0.1, tau = 1)
  
  # Compute hazard with constant baseline
  hazard_constant <- 
    calculate_hazard(
      t, x, betas, is_time_varying = FALSE, baseline_type = "constant")
  
  # Manually calculate expected hazard:
  # eta_0 = 0.5*0.5 + 1.0*1 + (-0.5)*(-0.5) + 0.2*0.3 + (-0.1)*0.2 + 0.1*0.5*1.0
  # hazard = exp(eta_0 + W * tau) * constant_baseline = exp(eta_0 + 1 * 1) * 1
  eta_0 <- 
    0.5 * 0.5 + 1.0 * 1 + 
    (-0.5) * (-0.5) + 0.2 * 0.3 + 
    (-0.1) * 0.2 + 0.1 * 0.5 * 1.0
  expected_hazard <- exp(eta_0 + 1 * 1) * 1
  expect_equal(
    as.numeric(hazard_constant), expected_hazard, tolerance = 1e-6)
  
})



# --------------------------
# TODO: Additional Tests
# --------------------------


# TODO: Test read_single_simulation_data function
# This should load datasets correctly from the saved paths and test edge cases 
# like missing or corrupted files.

# TODO: Test generate_and_save_data function
# This should generate the correct dataset structure and save it to the correct location.
# Additional tests might include verifying the saved file's integrity.
