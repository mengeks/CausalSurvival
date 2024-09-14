# Load required libraries
library(survival)
library(tidyverse)
library(jsonlite)  # For reading JSON files
source("R/datagen-helper.R")  # Helper functions for loading data
source("R/cox-loglik.R")      # Custom Cox model fitting functions
source("R/non-time-varying-estimate.R")  # Estimation functions (including DINA)

# Read the test configuration from the JSON file
config <- 
  fromJSON("tests/config-test-DINA.json")

R <- config$experiment$R
n <- config$experiment$n
is_time_varying <- config$experiment$is_time_varying
methods <- config$experiment$methods

# Initialize vectors to store tau estimates for DINA method
tau_estimates_DINA <- numeric(R)
time_taken_DINA <- numeric(R)

# Start experiment (DINA method only)
for (i in 1:R) {
  # Simulate or load the i-th dataset
  loaded_data <- read_single_simulation_data(
    n = n, R = R, is_time_varying = is_time_varying, i = i
  )
  single_data <- loaded_data$data
  tau_true <- loaded_data$params$tau
  light_censoring <- loaded_data$params$light_censoring
  
  # 1. DINA method
  if (methods$DINA$enabled) {
    # Randomly split the dataset into two folds
    folds <- sample(1:2, size = nrow(single_data), replace = TRUE)
    fold1 <- single_data[folds == 1, ]
    fold2 <- single_data[folds == 2, ]
    
    # Measure time for DINA estimation
    start_time <- Sys.time()
    tau_est_DINA <- DINA_estimation(
      fold1, fold2,
      nuisance_method = methods$DINA$nuisance_method,
      final_model_method = methods$DINA$final_model_method
    )
    end_time <- Sys.time()
    
    # Save estimates and time
    tau_estimates_DINA[i] <- tau_est_DINA
    time_taken_DINA[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
}

# Combine and save results for the test run
output_dir <- paste0("data/outputs/test-DINA/n_", n, "_R_", R)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save test results to an RDS file
test_results <- list(
  tau_estimates_DINA = tau_estimates_DINA,
  time_taken_DINA = time_taken_DINA,
  tau_true = tau_true
)

saveRDS(
  test_results, 
  file = file.path(output_dir, "test_results.rds")
)

cat("Test results saved to", file.path(output_dir, "test_results.rds"), "\n")

source("R/result-summaries.R")

calculate_bias_se_mse(
  tau_estimates = test_results$tau_estimates_DINA, 
  tau_true = test_results$tau_true)
