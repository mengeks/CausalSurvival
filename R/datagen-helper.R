library(simsurv)
library(dplyr)
library(tidyverse)
library(parallel)  # Load parallel here

# Helper function to generate covariates
generate_covariates <- function(n, p, is_time_varying) {
  X <- matrix(runif(n * p, min = -1, max = 1), n, p)
  
  if (is_time_varying) {
    A <- rexp(n, rate = exp(X[, 2] + X[, 3]))
    covariates <- data.frame(id = 1:n, X = X, A = A)
  } else {
    expit <- function(x) 1 / (1 + exp(-x))
    W <- rbinom(n, 1, prob = expit(X[, 2] + X[, 3]))
    covariates <- data.frame(id = 1:n, X = X, W = W)
  }
  
  return(covariates)
}

# Helper function to generate censoring times
generate_censoring_times <- function(n, light_censoring, lambda_C) {
  if (light_censoring) {
    return(rep(20, n))  # Large constant for censoring
  } else {
    return(rexp(n, rate = lambda_C))  # Regular censoring
  }
}

# Baseline hazard function
# Function to calculate the linear predictor (eta_0) based on eta_type
calculate_eta <- function(x, betas, eta_type = "linear-interaction") {
  # Extract relevant covariates
  X1 <- x[, "X.1"]
  X2 <- x[, "X.2"]
  X3 <- x[, "X.3"]
  X4 <- x[, "X.4"]
  X5 <- x[, "X.5"]
  
  # Calculate eta_0 based on the type of interaction
  if (eta_type == "linear-interaction") {
    # Linear-Interaction form
    eta_0 <- X1 * betas["beta1"] +
      X2 * betas["beta2"] + 
      X3 * betas["beta3"] + 
      X4 * betas["beta4"] + 
      X5 * betas["beta5"] + 
      betas["delta"] * X1 * X2
    
  } else if (eta_type == "log") {
    # Log form
    eta_0 <- 2 * log(1 + exp(X1 + X2 + X3))
    
  } else {
    stop("Unsupported eta_type")
  }
  
  return(eta_0)
}

# Function to calculate the baseline hazard based on baseline_type
calculate_baseline_hazard <- function(t, baseline_type = "cosine") {
  if (baseline_type == "linear") {
    baseline_hazard <- t
  } else if (baseline_type == "constant") {
    baseline_hazard <- 1
  } else {  # Default is "cosine"
    baseline_hazard <- (cos(t * 3) + 1) / 2
  }
  
  return(baseline_hazard)
}

# Main function to calculate the hazard
calculate_hazard <- function(t, x, betas, is_time_varying, baseline_type = "cosine", eta_type = "linear-interaction") {
  
  # Calculate the linear predictor eta_0
  eta_0 <- calculate_eta(x, betas, eta_type)
  
  # Calculate the baseline hazard
  baseline_hazard <- calculate_baseline_hazard(t, baseline_type)
  
  # Calculate the hazard based on whether time-varying or not
  if (is_time_varying) {
    hazard <- exp(eta_0 + (t >= x[, "A"]) * betas["tau"]) * baseline_hazard
  } else {
    hazard <- exp(eta_0 + x[, "W"] * betas["tau"]) * baseline_hazard
  }
  
  return(hazard)
}



# Main function to generate simulated data
generate_simulated_data <- function(n, 
                                    is_time_varying = FALSE, 
                                    light_censoring = FALSE,
                                    lambda_C = 0.1,
                                    tau = 1,
                                    p = 5,  
                                    beta = rep(1, 5),  
                                    delta = 0.5,
                                    baseline_type = "cosine",
                                    eta_type = "linear-interaction") {  
  
  # Step 1: Generate covariates
  covariates <- generate_covariates(n, p, is_time_varying)
  
  # Step 2: Generate censoring times
  C <- generate_censoring_times(n, light_censoring, lambda_C)
  
  # Step 3: Define the baseline hazard function
  hazard_function <- function(t, x, betas, ...) {
    calculate_hazard(t, x, betas, is_time_varying, baseline_type,eta_type)  # PASSING BASELINE_TYPE
  }
  
  
  # Step 4: Simulate survival data
  simulated_data <- simsurv(
    hazard = hazard_function,
    x = covariates,
    interval = c(1e-22, 500),
    betas = c(beta1 = beta[1], 
              beta2 = beta[2], 
              beta3 = beta[3], 
              beta4 = beta[4], 
              beta5 = beta[5], 
              delta = delta, 
              tau = tau), 
    maxt = 10  # Maximum follow-up time
  )
  
  # Step 5: Merge covariates with simulated data
  simulated_data <- merge(simulated_data, covariates, by = "id")
  
  # Step 6: Determine observed times and event indicators
  simulated_data <- simulated_data %>%
    mutate(U = pmin(eventtime, C),  # Observed time is the minimum of event time and censoring time
           Delta = as.numeric(eventtime <= C))  # Indicator for event before censoring
  
  # Step 7: Rename eventtime to T for clarity
  simulated_data <- simulated_data %>%
    rename(T = "eventtime")
  
  # Step 8: Combine with censoring times
  simulated_data <- cbind(simulated_data, C)
  
  return(simulated_data)
}


## Read the data
read_single_simulation_data <-
  function(n, R, is_time_varying, i, folder_name = "data/simulated") {
  
  # Determine the correct subfolder based on is_time_varying
  subfolder <- if (is_time_varying) {
    "time-varying"
  } else {
    "non-time-varying"
  }
  
  # Define the path to the folder containing the datasets
  path_for_sim_data <- here::here(
    folder_name, subfolder, paste0("sim_data_n_", n, "_R_", R)
  )
  
  # Define the file name based on the iteration number (i) and the seed value
  seed_value <- 123 + 11 * i
  file_name <- paste0("sim_data_", i, "_seed_", seed_value, ".rds")
  
  # Full path to the specific file
  file_path <- file.path(path_for_sim_data, file_name)
  
  # Check if the file exists before trying to read it
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  
  # Read the .rds file
  data <- readRDS(file_path)
  
  cat("Loaded dataset", i, "from", file_path, "\n")
  
  return(data)
}



# Function to generate and save datasets with params
generate_and_save_data <- function(i, n, is_time_varying, path_for_sim_data, params) {
  print(paste("Generating dataset", i, "for n =", n, "and time_varying =", is_time_varying))
  
  seed_value <- 123 + 11 * i
  set.seed(seed_value)
  
  # Generate one dataset
  # Inside generate_and_save_data
  simulated_data_i_n <- generate_simulated_data(
    n = n,
    is_time_varying = is_time_varying, 
    light_censoring = params$light_censoring,
    lambda_C = params$lambda_C,
    tau = params$tau,
    p = params$p,
    beta = params$beta,
    delta = params$delta,
    eta_type = params$eta_type,  # Pass eta_type from params
    baseline_type = params$baseline_type  # Pass baseline_type from params
  )
  
  # Combine the dataset with the simulation parameters (metadata)
  dataset_with_params <- list(
    data = simulated_data_i_n,
    params = params
  )
  
  # Define the file name and path
  file_name <- paste0("sim_data_", i, "_seed_", seed_value, ".rds")
  file_path <- file.path(path_for_sim_data, file_name)
  
  # Save the generated dataset with parameters
  saveRDS(dataset_with_params, file_path)
  
  cat("Saved dataset", i, "to", file_path, "\n")
}


run_simulation <- function(n_list, R, is_time_varying_range, params, cores = detectCores()) {
  for (n in n_list) {
    for (is_time_varying in is_time_varying_range) {
      # Existing logic for dataset generation
      folder_name <- if (is_time_varying) "data/simulated/time-varying" else "data/simulated/non-time-varying"
      path_for_sim_data <- here::here(
        folder_name, paste0(params$eta_type, "_", params$baseline_type), paste0("sim_data_n_", n, "_R_", R)
      )
      # path_for_sim_data <- here::here(folder_name, paste0("sim_data_n_", n, "_R_", R))
      dir.create(path_for_sim_data, showWarnings = FALSE, recursive = TRUE)
      # print(path_for_sim_data)
      for (group_start in seq(1, R, by = 20)) {
        group_end <- min(group_start + 19, R)
        time_taken <- system.time({
          mclapply(group_start:group_end, generate_and_save_data, 
                   n = n, 
                   is_time_varying = is_time_varying, 
                   path_for_sim_data = path_for_sim_data, 
                   params = params,  # Pass params with eta_type and baseline_type included
                   mc.cores = cores)
        })
        print(paste("Time taken for iterations", group_start, "to", group_end, ":", time_taken[3], "seconds"))
      }
    }
  }
}

