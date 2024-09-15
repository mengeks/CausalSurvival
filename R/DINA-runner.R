# Load required libraries
library(jsonlite)
library(tools)

# Function to handle Cox model estimation for different specifications
run_cox_estimation <- function(single_data, methods_cox) {
  results <- list()
  if (methods_cox$enabled) {
    for (spec in methods_cox$model_specifications) {
      start_time <- Sys.time()
      
      # Compute the estimate
      tau_est_cox <- cox_model_estimation(single_data, model_spec = spec)
      end_time <- Sys.time()
      
      # Calculate time taken
      time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      # Store result for this specification
      results[[spec]] <- list(
        tau_estimate = tau_est_cox,
        time_taken = time_taken
      )
    }
  }
  return(results)
}


# Function to handle S-Lasso estimation with light_censoring
run_slasso_estimation <- function(single_data, methods_slasso, light_censoring) {
  results <- list()
  if (methods_slasso$enabled) {
    start_time <- Sys.time()
    
    # Compute the estimate
    tau_est_slasso <- slasso_estimation(single_data, light_censoring = light_censoring)
    end_time <- Sys.time()
    
    # Calculate time taken
    time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Store result
    results <- list(
      tau_estimate = tau_est_slasso,
      time_taken = time_taken
    )
  }
  return(results)
}


# Function to handle DINA estimation with multiple configurations and light_censoring using K-fold cross-fitting
run_DINA_estimation <- function(single_data, methods_DINA, light_censoring, K = 2) {
  results <- list()
  if (methods_DINA$enabled) {
    # Create K-folds
    folds <- cut(seq(1, nrow(single_data)), breaks = K, labels = FALSE)
    
    # Loop over both nuisance methods and final model methods
    for (nuisance_method in methods_DINA$nuisance_method) {
      for (final_model_method in methods_DINA$final_model_method) {
        config_name <- paste(nuisance_method, final_model_method, sep = "_")
        
        start_time <- Sys.time()
        tau_estimates <- numeric(K)
        
        # Perform cross-fitting for K folds
        for (k in 1:K) {
          # Split the data: fold_k is the test set, all other folds are the training set
          fold_test <- single_data[folds == k, ]
          fold_train <- single_data[folds != k, ]
          
          # Estimate nuisance for fold_test using fold_train
          fold_test_nuisance <- DINA_estimate_nuisance(fold_train, fold_test, nuisance_method = nuisance_method, light_censoring = light_censoring)
          
          # Estimate tau for fold_test
          tau_estimates[k] <- fit_final_model(fold_test_nuisance, final_model_method)
        }
        
        # Average tau across K folds
        tau_est_DINA <- mean(tau_estimates)
        end_time <- Sys.time()
        
        # Calculate time taken
        time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        # Store result for this configuration
        results[[config_name]] <- list(
          tau_estimate = tau_est_DINA,
          time_taken = time_taken
        )
      }
    }
  }
  return(results)
}


# Main function to run the experiment
run_experiment <- function(json_file) {
  # Read experiment configuration from JSON
  config <- fromJSON(json_file)
  
  # Extract experiment parameters
  R <- config$experiment$R
  n <- config$experiment$n
  is_time_varying <- config$experiment$is_time_varying
  methods <- config$experiment$methods
  K <- ifelse(is.null(config$experiment$K), 2, config$experiment$K)  # Default K is 2 if not specified
  
  eta_type <- config$experiment$eta_type  # Extract eta_type from config
  baseline_type <- config$experiment$baseline_type  # Extract baseline_type from config
  
  # Initialize vectors to store tau estimates for each method
  tau_estimates_cox <- list()
  tau_estimates_slasso <- numeric(R)
  tau_estimates_DINA <- list()  # Now this is a list to handle different configurations
  
  # Define the output directory based on the input parameters, including eta_type and baseline_type
  json_file_name <- file_path_sans_ext(basename(json_file))  # Get the name of the JSON file without the extension
  eta_type_folder_name <- paste0(eta_type, "_", baseline_type)  # Folder name based on eta_type and baseline_type
  output_dir <- paste0("data/outputs/replicate-DINA/n_", n, "_R_", R, "/", eta_type_folder_name, "/", json_file_name)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Record time for each method
  time_taken <- list(cox = list(), slasso = numeric(R), DINA = list())
  
  # Start experiment loop
  for (i in 1:R) {
    # Load the i-th simulated dataset
    loaded_data <- read_single_simulation_data(
      n = n, 
      R = R, 
      is_time_varying = is_time_varying, 
      i = i, 
      eta_type = eta_type,  # Pass eta_type from config
      baseline_type = baseline_type  # Pass baseline_type from config
    )
    single_data <- loaded_data$data
    tau_true <- loaded_data$params$tau
    light_censoring <- loaded_data$params$light_censoring  # Extract light_censoring from params
    
    # Run Cox model estimation only if the cox method is specified in the JSON
    if (!is.null(methods$cox) && methods$cox$enabled) {
      cox_results <- run_cox_estimation(single_data, methods$cox)
      
      
      # Store the returned results in the appropriate slots
      for (spec in methods$cox$model_specifications) {
        if (is.null(tau_estimates_cox[[spec]])) {
          tau_estimates_cox[[spec]] <- numeric(R)
          time_taken$cox[[spec]] <- numeric(R)
        }
        
        # Store the estimate and time in the appropriate spot for the current iteration
        # tau_estimates_cox[[spec]][i] <- cox_results$tau_estimate
        # time_taken$cox[[spec]][i] <- cox_results$time_taken
        tau_estimates_cox[[spec]][i] <- cox_results[[spec]]$tau_estimate
        time_taken$cox[[spec]][i] <- cox_results[[spec]]$time_taken
      }
    }
    
    # Run S-Lasso estimation with light_censoring
    if (!is.null(methods$slasso) && methods$slasso$enabled) {
      slasso_results <- run_slasso_estimation(single_data, methods$slasso, light_censoring)
      
      # Store the estimate and time for the current iteration
      tau_estimates_slasso[i] <- slasso_results$tau_estimate
      time_taken$slasso[i] <- slasso_results$time_taken
    }
    
    # Run DINA estimation with light_censoring and K-fold cross-fitting
    if (!is.null(methods$DINA) && methods$DINA$enabled) {
      DINA_results <- run_DINA_estimation(single_data, methods$DINA, light_censoring, K)
      
      # Store the estimates and time for each configuration
      for (config_name in names(DINA_results$tau_estimates)) {
        if (is.null(tau_estimates_DINA[[config_name]])) {
          tau_estimates_DINA[[config_name]] <- numeric(R)
          time_taken$DINA[[config_name]] <- numeric(R)
        }
        
        # Store the estimate and time for the current configuration and iteration
        tau_estimates_DINA[[config_name]][i] <- DINA_results[[config_name]]$tau_estimate
        time_taken$DINA[[config_name]][i] <- DINA_results[[config_name]]$time_taken
      }
    }
  }
  
  # Combine results into a list
  results <- list(
    tau_estimates_cox = tau_estimates_cox,
    tau_estimates_slasso = tau_estimates_slasso,
    tau_estimates_DINA = tau_estimates_DINA,
    time_taken = time_taken,
    tau_true = tau_true
  )
  
  # Save the results to an RDS file
  result_file <- file.path(output_dir, "results.rds")
  saveRDS(results, result_file)
  
  # Output message
  cat("Results saved to", result_file, "\n")
}
