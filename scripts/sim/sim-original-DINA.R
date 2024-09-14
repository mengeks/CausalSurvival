# Load required libraries
library(survival)
library(tidyverse)
library(jsonlite) # For reading JSON files
source("R/datagen-helper.R")  # Assuming this function helps load the data
source("R/cox-loglik.R")      # Assuming this has `fit_custom_cox_model`
source("R/lasso.R")
source("R/non-time-varying-estimate.R")

# Read experiment configuration from JSON
config <- 
  fromJSON("scripts/sim/config-original-DINA.json")

R <- config$experiment$R
n <- config$experiment$n
is_time_varying <- config$experiment$is_time_varying
methods <- config$experiment$methods

# Initialize vectors to store tau estimates for each method
tau_estimates_cox <- list()
tau_estimates_slasso <- numeric(R)
tau_estimates_DINA <- numeric(R)

# Record time for each method
time_taken <- list(cox = list(), slasso = numeric(R), DINA = numeric(R))
# Start experiment
for (i in 1:R) {
  # Read the i-th simulated dataset
  loaded_data <- read_single_simulation_data(
    n = n, R = R, is_time_varying = is_time_varying, i = i
  )
  single_data <- loaded_data$data
  tau_true <- loaded_data$params$tau
  
  # 1. Method 1: Cox Model with Different Specifications
  if (methods$cox$enabled) {
    for (spec in methods$cox$model_specifications) {
      start_time <- 
        Sys.time()
      
      tau_est_cox <- 
        cox_model_estimation(single_data, model_spec = spec)
      end_time <- 
        Sys.time()
      
      # Save estimates and time
      if (is.null(tau_estimates_cox[[spec]])) {
        tau_estimates_cox[[spec]] <- numeric(R)
      }
      tau_estimates_cox[[spec]][i] <- tau_est_cox
      
      time_taken$cox[[spec]][i] <- 
        as.numeric(
          difftime(end_time, start_time, 
                   units = "secs"))
    }
  }
  
  # 2. Method 1.5: S-Lasso
  if (methods$slasso$enabled) {
    start_time <- Sys.time()
    tau_est_slasso <- slasso_estimation(single_data)
    end_time <- Sys.time()
    
    # Save estimates and time
    tau_estimates_slasso[i] <- tau_est_slasso
    time_taken$slasso[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
  
  # 3. Method 2: DINA
  if (methods$DINA$enabled) {
    # Randomly split the dataset into two folds
    folds <- sample(1:2, size = nrow(single_data), replace = TRUE)
    fold1 <- single_data[folds == 1, ]
    fold2 <- single_data[folds == 2, ]
    
    start_time <- Sys.time()
    tau_est_DINA <- DINA_estimation(
      fold1, fold2,
      nuisance_method = methods$DINA$nuisance_method,
      final_model_method = methods$DINA$final_model_method
    )
    end_time <- Sys.time()
    
    # Save estimates and time
    tau_estimates_DINA[i] <- tau_est_DINA
    time_taken$DINA[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }
}

# Combine and save results
output_dir <- paste0("data/outputs/non-time-varying-cox/n_", n, "_R_", R)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save results in a list for all methods
results <- list(
  tau_estimates_cox = tau_estimates_cox,
  tau_estimates_slasso = tau_estimates_slasso,
  tau_estimates_DINA = tau_estimates_DINA,
  time_taken = time_taken,
  tau_true = tau_true
)

# Save results to an RDS file
saveRDS(
  results, 
  file = file.path(output_dir, "results.rds"))

cat("Results saved to", file.path(output_dir, "results.rds"), "\n")
