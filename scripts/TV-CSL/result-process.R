source("R/data-handler.R")

#' Load Experiment Configuration
#'
#' Loads the experiment configuration from a JSON file.
#' 
#' @param json_file The path to the JSON configuration file.
#' @return A list containing the experiment configuration.
load_experiment_config <- function(json_file) {
  config <- fromJSON(json_file)
  return(config)
}


# Helper function to read individual CSV files
read_single_iteration_result <- function(csv_file) {
  df <- read.csv(csv_file)
  return(df)
}

# generate_result_csv_file <- function(results_dir, eta_type_folder_name, n, i, seed_value) {
#   result_csv_file <- paste0(
#     results_dir, 
#     "/", eta_type_folder_name, 
#     "-n_", n, 
#     "-iteration_", i, 
#     "-seed_", seed_value, ".csv"
#   )
#   return(result_csv_file)
# }

# Function to aggregate all CSV files and calculate final metrics
process_all_iterations <- function(config, results_dir) {
  n <- config$n
  methods <- config$methods
  eta_type <- config$eta_type
  HTE_type <- config$HTE_type
  R <- config$R
  
  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled

  
  all_results <- list()
  all_times <- list()
  
  for (i in 1:R) {
    seed_value <- 123 + 11 * i
    
    result_csv_file <- generate_output_path(
      is_running_cox = is_running_cox,
      is_running_lasso = is_running_lasso,
      is_running_TV_CSL = is_running_TV_CSL,
      eta_type = eta_type,
      HTE_type = HTE_type,
      n = n,
      i = i,
      seed_value = seed_value
    )
    
    
    if (file.exists(result_csv_file)) {
      iteration_result <- read_single_iteration_result(result_csv_file)
      all_results[[i]] <- iteration_result
    } else {
      cat("Warning: File not found for iteration", i, "\n")
    }
  }
  
  combined_results <- do.call(rbind, all_results)
  combined_results$Specification <- 
    gsub("S_lasso", "S-lasso", combined_results$Specification)
  combined_results$Specification <- 
    gsub("T_lasso", "T-lasso", combined_results$Specification)
  
  
  aggregated_metrics <- combined_results %>%
    group_by(Method, Specification) %>%
    summarise(
      MSE = mean(MSE_Estimate)
    )
  
  return(aggregated_metrics)
}

process_results_to_csv <- function(json_file) {
  
  config <- load_experiment_config(json_file)
  n <- config$n
  
  
  aggregated_metrics <- 
    process_all_iterations(
      config=config, 
      results_dir=RESULTS_DIR
    )
  
  output_csv_dir <- "scripts/TV-CSL/tables"
  
  eta_type_folder_name <- 
    paste0(config$eta_type, "_", config$HTE_type)
  
  methods <- config$methods
  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  method_setting <- paste0(
    ifelse(is_running_cox, "cox_", ""),
    ifelse(is_running_lasso, "lasso_", ""),
    ifelse(is_running_TV_CSL, "TV-CSL_", "")
  )
  
  metrics_csv_file <- 
    file.path(output_csv_dir, 
              paste0(method_setting, eta_type_folder_name,"_n_", n, "_est_quality.csv") )
  
  write.csv(
    aggregated_metrics, 
    metrics_csv_file, 
    row.names = FALSE)
  
  cat("Aggregated results saved to", metrics_csv_file, "\n")
}
