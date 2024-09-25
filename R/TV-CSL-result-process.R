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

#' Generate Directory Path
#'
#' Generates the directory path for saving data based on experiment configuration.
#' 
#' @param base_dir The base directory where results should be saved.
#' @param config A list containing the experiment configuration parameters (n, R, eta_type, baseline_type).
#' @return A string representing the full directory path for saving data.
generate_directory_path <- function(base_dir, config) {
  n <- config$n
  eta_type <- config$eta_type  # Extract eta_type from config
  baseline_type <- config$baseline_type  # Extract baseline_type from config
  
  # Create folder name based on eta_type and baseline_type
  eta_type_folder_name <- 
    paste0(eta_type, "_", baseline_type)
  
  # Generate the full directory path without json_file_name
  dir_path <- 
    paste0(base_dir, "/", eta_type_folder_name, "/n_", n)
  
  return(dir_path)
}

#' Generate Output Directory for Saving Results
#'
#' Generates the output directory path for saving experiment results.
#' 
#' @param config A list containing the experiment configuration parameters (n, R, eta_type, baseline_type).
#' @return A string representing the output directory path.
generate_output_dir <- function(config) {
  base_dir <- "data/outputs/TV-CSL"  # Base directory for saving output data
  output_dir <- generate_directory_path(base_dir, config)  # Use common function
  return(output_dir)
}

#' Ensure Output Directory for CSV Files
#'
#' Ensures that the directory for saving CSV files exists. If not, it creates the directory.
#' 
#' @param config A list containing the experiment configuration parameters (n, R, eta_type, baseline_type).
#' @param R The value of R used in the experiment, defaults to NULL.
#' @return A string representing the output directory path for CSV files.
ensure_output_csv_dir <- function(config, R = NULL) {
  base_dir <- "tables/TV-CSL"  # Base directory for saving CSV tables
  output_csv_dir <- generate_directory_path(base_dir, config)  # Generate directory path
  
  if (!is.null(R)) {
    output_csv_dir <- file.path(output_csv_dir, paste0("R_", R))  # Append R to the path if provided
  }
  
  # Create the directory if it doesn't exist
  dir.create(output_csv_dir, recursive = TRUE, showWarnings = FALSE)
  
  return(output_csv_dir)
}


#' Read Results from Output Directory
#'
#' Reads the results from the output directory.
#' 
#' @param output_dir The directory from which to read the results.
#' @param R The specific replication number (R) to append to the results file name.
#' @return A list containing the results.
read_results <- function(output_dir, R = NULL) {
  if (is.null(R)) {
    results_file <- here::here(output_dir, "results.rds")
  } else {
    results_file <- here::here(output_dir, paste0("R_", R, "_results.rds"))
  }
  
  results <- readRDS(file = results_file)
  return(results)
}


# Function to calculate bias, SE, and MSE for Cox, S-Lasso, and DINA methods
calculate_metrics <- function(results, tau_true) {
  # Calculate metrics for Cox
  if (length(results$tau_estimates_cox) > 0) {
    cox_metrics_list <- 
      lapply(results$tau_estimates_cox, 
             calculate_bias_se_mse, 
             tau_true = tau_true)
  } else {
    cox_metrics_list <- list()  # Empty if no Cox results
  }
  
  # Calculate metrics for S-Lasso
  if (length(results$tau_estimates_slasso) > 0) {
    slasso_metrics <- 
      calculate_bias_se_mse(
        results$tau_estimates_slasso, tau_true)
  } else {
    slasso_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA if no S-Lasso results
  }
  
  # Calculate metrics for DINA
  if (length(results$tau_estimates_DINA) > 0) {
    # DINA_estimates <- results$tau_estimates_DINA$cox_custom_cox  # Access the sublist
    # DINA_metrics <- calculate_bias_se_mse(DINA_estimates, tau_true)
    DINA_metrics <- lapply(
      results$tau_estimates_DINA, 
      calculate_bias_se_mse,
      tau_true = tau_true)
    # print(DINA_metrics)
  } else {
    DINA_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA if no DINA results
  }
  
  return(list(cox_metrics_list = cox_metrics_list, slasso_metrics = slasso_metrics, DINA_metrics = DINA_metrics))
}

# Function to prepare metrics for saving to CSV
prepare_metrics_dataframe <- function(cox_metrics_list, slasso_metrics = NULL, DINA_metrics = NULL) {
  
  # Prepare Cox metrics
  if (length(cox_metrics_list) > 0) {
    cox_bias <- sapply(cox_metrics_list, `[[`, "bias")
    cox_se <- sapply(cox_metrics_list, `[[`, "se")
    cox_mse <- sapply(cox_metrics_list, `[[`, "mse")
    cox_specs <- names(cox_metrics_list)
  } else {
    cox_bias <- c()
    cox_se <- c()
    cox_mse <- c()
    cox_specs <- c()
  }
  
  # Initialize empty vectors for S-Lasso and DINA in case they are NULL
  slasso_bias <- slasso_se <- slasso_mse <- NA
  slasso_specs <- NA
  
  DINA_bias <- DINA_se <- DINA_mse <- c()
  DINA_specs <- c()
  
  # Conditionally add S-Lasso metrics if provided
  if (!is.null(slasso_metrics)) {
    slasso_bias <- if (!is.null(slasso_metrics["bias"])) slasso_metrics["bias"] else NA
    slasso_se <- if (!is.null(slasso_metrics["se"])) slasso_metrics["se"] else NA
    slasso_mse <- if (!is.null(slasso_metrics["mse"])) slasso_metrics["mse"] else NA
    slasso_specs <- "S-Lasso"
  }
  
  # Conditionally add DINA metrics if provided
  if (!is.null(DINA_metrics) && length(DINA_metrics) > 0) {
    DINA_bias <- sapply(DINA_metrics, `[[`, "bias")
    DINA_se <- sapply(DINA_metrics, `[[`, "se")
    DINA_mse <- sapply(DINA_metrics, `[[`, "mse")
    DINA_specs <- names(DINA_metrics)
  }
  
  # Combine the metrics for all methods into a single dataframe
  metrics_df <- data.frame(
    Method = c(rep("Cox", length(cox_specs)), slasso_specs, rep("DINA", length(DINA_specs))),
    Specification = c(cox_specs, slasso_specs, DINA_specs),
    Bias = c(cox_bias, slasso_bias, DINA_bias),
    SE = c(cox_se, slasso_se, DINA_se),
    MSE = c(cox_mse, slasso_mse, DINA_mse),
    stringsAsFactors = FALSE
  )
  
  return(metrics_df)
}

# Function to prepare time metrics for saving to CSV
prepare_time_metrics_dataframe <- function(results) {
  cox_time_list <- sapply(results$time_taken$cox, mean)
  slasso_time <- mean(results$time_taken$slasso)
  DINA_time <- mean(results$time_taken$DINA)
  
  time_metrics_df <- data.frame(
    Method = c(rep("Cox", length(cox_time_list)), "S-Lasso", "DINA"),
    Specification = c(names(cox_time_list), NA, NA),
    Avg_Time = c(cox_time_list, slasso_time, DINA_time)
  )
  return(time_metrics_df)
}

# Function to save metrics to CSV
save_metrics_to_csv <- function(metrics_df, time_metrics_df, output_csv_dir) {
  metrics_csv_file <- file.path(output_csv_dir, "aggregated_metrics.csv")
  write.csv(metrics_df, metrics_csv_file, row.names = FALSE)
  
  time_metrics_csv_file <- file.path(output_csv_dir, "aggregated_time_metrics.csv")
  write.csv(time_metrics_df, time_metrics_csv_file, row.names = FALSE)
  
  cat("Aggregated results saved to CSV files in:", output_csv_dir, "\n")
}

process_results_to_csv <- function(json_file) {
  # Load experiment configuration
  config <- load_experiment_config(json_file)
  
  # Generate output directory based on the experiment parameters
  output_dir <- 
    generate_output_dir(config)
  
  # Read the results from the output directory
  results <- read_results(output_dir, R=config$R)
  
  # Extract tau_true
  tau_true <- results$tau_true
  
  # Calculate metrics for Cox, S-Lasso, and DINA methods
  metrics <- 
    calculate_metrics(results, tau_true)
  # print(metrics$DINA_metrics)
  
  # Prepare data for saving to CSV
  metrics_df <- 
    prepare_metrics_dataframe(
      metrics$cox_metrics_list,
      slasso_metrics = NULL, 
      DINA_metrics = NULL)
  time_metrics_df <-
    prepare_time_metrics_dataframe(results)
  
  output_csv_dir <- 
    ensure_output_csv_dir(
      config, R=config$R)
  
  # Save metrics and time metrics to CSV files
  save_metrics_to_csv(
    metrics_df, time_metrics_df, output_csv_dir)
}
