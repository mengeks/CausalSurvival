
#' Run Cox Model Estimation for Different Specifications
#'
#' This function runs Cox model estimation for the provided dataset 
#' using different model specifications from the \code{methods_cox}.
#'
#' @param single_data A data frame containing the survival data.
#' @param methods_cox A list containing the model specification and time-varying flag for Cox model.
#' @return A list of results containing the tau estimates and time taken for each model specification.
#' @export
run_cox_estimation <- function(single_data, methods_cox) {
  results <- list()
  
  if (methods_cox$enabled) {
    for (spec in methods_cox$model_specifications) {
      for (run_time_varying in methods_cox$run_time_varying) {
        
        config_name <- paste(spec, run_time_varying, sep = "_")
        
        start_time <- Sys.time()
        
        # Create a temporary methods_cox object that contains the current specification and run_time_varying
        current_methods_cox <- methods_cox
        current_methods_cox$model_spec <- spec
        current_methods_cox$run_time_varying <- run_time_varying
        
        # Run the Cox model estimation using the current configuration
        tau_est_cox <- cox_model_estimation(single_data, current_methods_cox)
        
        end_time <- Sys.time()
        
        time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        # Save the result using config_name
        results[[config_name]] <- list(
          tau_estimate = tau_est_cox,
          time_taken = time_taken
        )
      }
    }
  }
  
  return(results)
}

#' Run a Single Iteration of the Experiment and Save Results to CSV
#'
#' This function runs the experiment for a single iteration and saves the results to a CSV file.
#'
#' @param i The iteration number to run.
#' @param json_file Path to the JSON configuration file.
#' @param verbose The level of verbosity (0 = default, 1 = progress info, 2 = detailed info).
run_experiment_iteration <- function(i, json_file, verbose = 0) {
  library(jsonlite)
  library(tools)
  source("R/data-reader.R")
  source("R/time-varying-estimate.R")
  
  if (verbose >= 1) cat("Running iteration", i, "\n")
  
  config <- fromJSON(json_file)
  
  n <- config$n
  R <- config$R
  is_time_varying <- config$is_time_varying
  methods <- config$methods
  K <- ifelse(is.null(config$K), 2, config$K)
  
  eta_type <- config$eta_type
  baseline_type <- config$baseline_type
  
  json_file_name <- file_path_sans_ext(basename(json_file))
  eta_type_folder_name <- paste0(eta_type, "_", baseline_type)
  seed_value <- 123 + 11 * i
  set.seed(seed_value)
  
  output_dir <- paste0("results/TV-CSL/", eta_type_folder_name, "-n_", n)
  
  if (verbose >= 2) {
    cat("Configuration Parameters:\n")
    cat("n:", n, "\nR:", R, "\nis_time_varying:", is_time_varying, "\neta_type:", eta_type, "\nbaseline_type:", baseline_type, "\n")
    cat("Seed value for iteration", i, ":", seed_value, "\n")
  }
  
  # Load the i-th simulated dataset
  start_time <- Sys.time()
  loaded_data <- read_single_simulation_data(
    n = n, 
    R = R, 
    is_time_varying = is_time_varying, 
    i = i, 
    eta_type = eta_type,  
    baseline_type = baseline_type
  )
  end_time <- Sys.time()
  
  if (verbose >= 1) cat("Time to load dataset:", as.numeric(difftime(end_time, start_time, units = "secs")), "seconds\n")
  
  single_data <- loaded_data$data
  tau_true <- loaded_data$params$tau
  light_censoring <- loaded_data$params$light_censoring
  
  # Run Cox model estimation
  tau_estimates_cox <- list()
  time_taken <- list()
  
  if (!is.null(methods$cox) && methods$cox$enabled) {
    start_time <- Sys.time()
    cox_results <- run_cox_estimation(single_data, methods$cox)
    end_time <- Sys.time()
    
    for (config_name in names(cox_results)) {
      tau_estimates_cox[[config_name]] <- cox_results[[config_name]]$tau_estimate
      time_taken[[config_name]] <- cox_results[[config_name]]$time_taken
    }
    
    if (verbose >= 1) cat("Time to run Cox model:", as.numeric(difftime(end_time, start_time, units = "secs")), "seconds\n")
    if (verbose >= 2) {
      cat("Cox Results:\n")
      print(tau_estimates_cox)
    }
  }
  
  # Create a data frame for the result
  result_df <- data.frame(
    Method = rep("Cox", length(tau_estimates_cox)),
    Specification = names(tau_estimates_cox),
    Tau_Estimate = unlist(tau_estimates_cox),
    Time_Taken = unlist(time_taken)
  )
  
  # Save the result of the i-th iteration as a CSV file
  result_csv_file <- paste0(output_dir, "-iteration_", i, "-seed_", seed_value, ".csv")
  write.csv(result_df, result_csv_file, row.names = FALSE)
  
  if (verbose >= 1) cat("Results for iteration", i, "saved to", result_csv_file, "\n")
}
