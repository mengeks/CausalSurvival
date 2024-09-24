# Load required libraries
library(jsonlite)
library(tools)
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

#' Main Function to Run Experiment
#'
#' This function runs the entire experiment loop based on the given configuration from a JSON file.
#' It handles loading the data, running the Cox model estimation, and saving the results.
#'
#' @param json_file Path to the JSON configuration file.
#' @export
run_experiment <- function(json_file) {
  config <- fromJSON(json_file)
  
  R <- config$R
  n <- config$n
  is_time_varying <- config$is_time_varying
  methods <- config$methods
  K <- ifelse(is.null(config$K), 2, config$K)
  
  eta_type <- config$eta_type
  baseline_type <- config$baseline_type
  
  tau_estimates_cox <- list()
  tau_estimates_DINA <- list()
  
  json_file_name <- file_path_sans_ext(basename(json_file))
  eta_type_folder_name <- paste0(eta_type, "_", baseline_type)
  # output_dir <- 
  #   paste0("data/outputs/replicate-DINA/n_", n, "_R_", R, "/", eta_type_folder_name, "/", json_file_name)
  output_dir <-  
    paste0("data/outputs/TV-CSL/", eta_type_folder_name, "/n_", n)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  result_file <- 
    file.path(output_dir, paste0("R_", R, "_results.rds") )
  print("result_file")
  print(result_file)
  
  time_taken <- list(cox = list(), DINA = list())
  
  for (i in 1:R) {
    loaded_data <- read_single_simulation_data(
      n = n, 
      R = R, 
      is_time_varying = is_time_varying, 
      i = i, 
      eta_type = eta_type,  
      baseline_type = baseline_type
    )
    
    single_data <- loaded_data$data
    tau_true <- loaded_data$params$tau
    light_censoring <- loaded_data$params$light_censoring
    
    if (!is.null(methods$cox) && methods$cox$enabled) {
      cox_results <- 
        run_cox_estimation(single_data, methods$cox)
      
      # for (spec in methods$cox$model_specifications) {
      for (config_name in names(cox_results)) {
        if (is.null(tau_estimates_cox[[config_name]])) {
          tau_estimates_cox[[config_name]] <- numeric(R)
          time_taken$cox[[config_name]] <- numeric(R)
        }
        
        tau_estimates_cox[[config_name]][i] <- cox_results[[config_name]]$tau_estimate
        time_taken$cox[[config_name]][i] <- cox_results[[config_name]]$time_taken
      }
    }
    
    # Uncomment when running S-Lasso or DINA
    # if (!is.null(methods$slasso) && methods$slasso$enabled) {
    #   slasso_results <- run_slasso_estimation(single_data, methods$slasso, light_censoring)
    #   tau_estimates_slasso[i] <- slasso_results$tau_estimate
    #   time_taken$slasso[i] <- slasso_results$time_taken
    # }
    
    # if (!is.null(methods$DINA) && methods$DINA$enabled) {
    #   DINA_results <- run_DINA_estimation(single_data, methods$DINA, light_censoring, K)
    #   for (config_name in names(DINA_results)) {
    #     if (is.null(tau_estimates_DINA[[config_name]])) {
    #       tau_estimates_DINA[[config_name]] <- numeric(R)
    #       time_taken$DINA[[config_name]] <- numeric(R)
    #     }
    #     tau_estimates_DINA[[config_name]][i] <- DINA_results[[config_name]]$tau_estimate
    #     time_taken$DINA[[config_name]][i] <- DINA_results[[config_name]]$time_taken
    #   }
    # }
  } 
  
  results <- list(
    tau_estimates_cox = tau_estimates_cox,
    tau_estimates_DINA = tau_estimates_DINA,
    time_taken = time_taken,
    tau_true = tau_true
  )
  
  saveRDS(results, result_file)
  cat("Results saved to", result_file, "\n")
}
