library(simsurv)
library(dplyr)
library(tidyverse)
library(parallel)  # Load parallel here

generate_simulated_data <- 
  function(n, 
    is_time_varying = FALSE, 
         light_censoring = FALSE,
         lambda_C = 0.1,
         tau = 1,
         p = 5,         # Number of predictors
         beta = rep(1,5),  # Coefficients for the linear part of eta_0(x)
         delta = 0.5) # Coefficient for the interaction term x1 * x2
    {  
  
  X <- matrix(runif(n * p, min = -1, max = 1), n, p)
  
  if (is_time_varying) {
    A <- rexp(n, rate = exp(X[,2] + X[,3]))
    covariates <- 
      data.frame(id = 1:n, 
                 X = X, 
                 A = A)
  } else {
    expit <- function(x) 1 / (1 + exp(-x))  # Logistic function
    W <- rbinom(
      n, 1, prob = expit(X[,2] + X[,3])
    ) 
    covariates <- 
      data.frame(id = 1:n, 
                 X = X, 
                 W = W)
  }
  
  # Generate censoring time C
  if (light_censoring) {
    C <- rep(20, n)  # Set C to a large constant
  } else {
    C <- rexp(n, rate = lambda_C)  # Regular censoring
  }
  
  
  baseline <- function(t, x, betas, ...) {
    # Extract relevant covariates
    X1 <- x[, "X.1"]
    X2 <- x[, "X.2"]
    X3 <- x[, "X.3"]
    X4 <- x[, "X.4"]
    X5 <- x[, "X.5"]
    
    # Calculate eta_0 using the correct covariates
    eta_0 <- X1 * betas["beta1"] + 
      X2 * betas["beta2"] + 
      X3 * betas["beta3"] + 
      X4 * betas["beta4"] + 
      X5 * betas["beta5"] + 
      betas["delta"] * X1 * X2
    
    # Calculate the hazard based on whether time-varying or not
    if (is_time_varying) {
      hazard <- exp(eta_0 + (t >= x[, "A"]) * betas["tau"]) * (cos(t * 3) + 1) / 2
    } else {
      hazard <- exp(eta_0 + x[, "W"] * betas["tau"]) * (cos(t * 3) + 1) / 2
    }
    
    return(hazard)
  }
  
  # Simulate survival data
  # Assume covariates has columns X.1, X.2, X.3, X.4, X.5, W, and possibly A if needed for time-varying hazard
  simulated_data <- simsurv(
    hazard = baseline,
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
  
  
  # Merge covariates with simulated data
  simulated_data <- 
    merge(simulated_data, 
          covariates, 
          by = "id")
  
  # Determine observed times and event indicators
  simulated_data <- simulated_data %>%
    mutate(U = pmin(eventtime, C),
           Delta = as.numeric(eventtime <= C))
  
  # Rename eventtime to T for clarity
  simulated_data <- simulated_data %>%
    rename(T = "eventtime")
  
  # Combine with censoring times
  simulated_data <- cbind(simulated_data, C)
  
  return(simulated_data)
}


## Read the data
read_single_simulation_data <- function(n, R, is_time_varying, i, folder_name = "data/simulated") {
  
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
  simulated_data_i_n <- generate_simulated_data(
    n = n,
    is_time_varying = is_time_varying, 
    light_censoring = params$light_censoring,
    lambda_C = params$lambda_C,
    tau = params$tau,
    p = params$p,
    beta = params$beta,
    delta = params$delta
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
      path_for_sim_data <- here::here(folder_name, paste0("sim_data_n_", n, "_R_", R))
      dir.create(path_for_sim_data, showWarnings = FALSE, recursive = TRUE)
      
      for (group_start in seq(1, R, by = 20)) {
        group_end <- min(group_start + 19, R)
        time_taken <- system.time({
          mclapply(group_start:group_end, generate_and_save_data, 
                   n = n, is_time_varying = is_time_varying, 
                   path_for_sim_data = path_for_sim_data, params = params, 
                   mc.cores = cores)
        })
        print(paste("Time taken for iterations", group_start, "to", group_end, ":", time_taken[3], "seconds"))
      }
    }
  }
}

