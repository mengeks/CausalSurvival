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

# Function to aggregate all CSV files and calculate final metrics
process_all_iterations <- function(config, results_dir) {
  n <- config$n
  eta_type <- config$eta_type
  CATE_type <- config$CATE_type
  # baseline_type <- config$baseline_type
  R <- config$R
  
  eta_type_folder_name <- 
    # paste0(eta_type, "_", baseline_type)
    paste0(eta_type, "_", CATE_type)
  
  all_results <- list()
  all_times <- list()
  
  for (i in 1:R) {
    seed_value <- 123 + 11 * i
    
    result_csv_file <- 
      paste0(results_dir, 
             "/",eta_type_folder_name, "-n_",n, "-iteration_", i, "-seed_", seed_value, ".csv")
    
    # Read the results of the i-th iteration
    if (file.exists(result_csv_file)) {
      iteration_result <- read_single_iteration_result(result_csv_file)
      all_results[[i]] <- iteration_result
    } else {
      cat("Warning: File not found for iteration", i, "\n")
    }
  }
  
  # Combine all results into a single dataframe
  combined_results <- do.call(rbind, all_results)
  
  # Now calculate the metrics like bias, SE, and MSE for each method and specification
  aggregated_metrics <- combined_results %>%
    group_by(Method, Specification) %>%
    summarise(
      MSE = mean(MSE_Estimate)
    )
    # summarise(
    #   Bias = mean(Tau_Estimate - 1),  # Assuming tau_true is 1 if not found
    #   SE = sd(Tau_Estimate),
    #   MSE = mean((Tau_Estimate - 1)^2)
    # )
  
  return(aggregated_metrics)
}

process_results_to_csv <- function(json_file) {
  # Load experiment configuration
  config <- load_experiment_config(json_file)
  n <- config$n
  
  # Generate result directories
  results_dir <- "results/TV-CSL"
  
  # Process all iterations
  aggregated_metrics <- 
    process_all_iterations(
      config=config, 
      results_dir=results_dir
    )
  
  # Ensure CSV directory
  output_csv_dir <- "tables/TV-CSL"
  
  eta_type_folder_name <- 
    paste0(config$eta_type, "_", config$CATE_type)
  metrics_csv_file <- 
    file.path(output_csv_dir, 
              paste0(eta_type_folder_name,"_n_", n, "_est_quality.csv") )
  write.csv(
    aggregated_metrics, 
    metrics_csv_file, 
    row.names = FALSE)
  
  cat("Aggregated results saved to", metrics_csv_file, "\n")
}
