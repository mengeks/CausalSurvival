# Load necessary libraries
library(tidyverse)
library(survival)
library(here)

# Load helper functions
source("R/datagen-helper.R")

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
  delta = 0.5
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

# --------------------------
# TODO: Additional Tests
# --------------------------

# TODO: Test generate_simulated_data function
# generate_simulated_data should be tested separately with different parameter settings 
# to ensure it works for both time-varying and non-time-varying scenarios.

# TODO: Test read_single_simulation_data function
# This should load datasets correctly from the saved paths and test edge cases 
# like missing or corrupted files.

# TODO: Test generate_and_save_data function
# This should generate the correct dataset structure and save it to the correct location.
# Additional tests might include verifying the saved file's integrity.
