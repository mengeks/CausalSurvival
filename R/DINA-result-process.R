# Function to load experiment configuration
load_experiment_config <- function(json_file) {
  config <- fromJSON(json_file)
  return(config)
}

# Function to generate the directory path, allowing for different base directories
generate_directory_path <- function(base_dir, config, json_file) {
  n <- config$experiment$n
  R <- config$experiment$R
  eta_type <- config$experiment$eta_type  # Extract eta_type from config
  baseline_type <- config$experiment$baseline_type  # Extract baseline_type from config
  
  json_file_name <- tools::file_path_sans_ext(basename(json_file))  # Get the name without extension
  eta_type_folder_name <- paste0(eta_type, "_", baseline_type)  # Create folder name based on eta_type and baseline_type
  
  # Generate the full directory path
  dir_path <- paste0(base_dir, "/n_", n, "_R_", R, "/", eta_type_folder_name, "/", json_file_name)
  
  return(dir_path)
}

# Function to generate the output directory path for saving results
generate_output_dir <- function(config, json_file) {
  base_dir <- "data/outputs/replicate-DINA"  # Base directory for saving output data
  output_dir <- generate_directory_path(base_dir, config, json_file)  # Use common function
  return(output_dir)
}

# Function to ensure output directory for saving CSV files
ensure_output_csv_dir <- function(config, json_file) {
  base_dir <- "tables/replicate-DINA"  # Base directory for saving CSV tables
  output_csv_dir <- generate_directory_path(base_dir, config, json_file)  # Use common function
  
  # Create the directory if it doesn't exist
  dir.create(output_csv_dir, recursive = TRUE, showWarnings = FALSE)
  
  return(output_csv_dir)
}


# Function to read results from the output directory
read_results <- function(output_dir) {
  results_file <- 
    here::here(output_dir, "results.rds")
  results <- readRDS(file = results_file)
  return(results)
}

# Function to calculate bias, SE, and MSE for Cox, S-Lasso, and DINA methods
calculate_metrics <- function(results, tau_true) {
  # Calculate metrics for Cox
  if (length(results$tau_estimates_cox) > 0) {
    cox_metrics_list <- lapply(results$tau_estimates_cox, calculate_bias_se_mse, tau_true = tau_true)
  } else {
    cox_metrics_list <- list()  # Empty if no Cox results
  }
  
  # Calculate metrics for S-Lasso
  if (length(results$tau_estimates_slasso) > 0) {
    slasso_metrics <- calculate_bias_se_mse(results$tau_estimates_slasso, tau_true)
  } else {
    slasso_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA if no S-Lasso results
  }
  
  # Calculate metrics for DINA
  if (length(results$tau_estimates_DINA) > 0) {
    DINA_estimates <- results$tau_estimates_DINA$cox_custom_cox  # Access the sublist
    DINA_metrics <- calculate_bias_se_mse(DINA_estimates, tau_true)
  } else {
    DINA_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA if no DINA results
  }
  
  return(list(cox_metrics_list = cox_metrics_list, slasso_metrics = slasso_metrics, DINA_metrics = DINA_metrics))
}

# Function to prepare metrics for saving to CSV
prepare_metrics_dataframe <- function(cox_metrics_list, slasso_metrics, DINA_metrics) {
  
  # Prepare Cox metrics
  if (length(cox_metrics_list) > 0) {
    cox_bias <- sapply(cox_metrics_list, function(x) x["bias"])
    cox_se <- sapply(cox_metrics_list, function(x) x["se"])
    cox_mse <- sapply(cox_metrics_list, function(x) x["mse"])
  } else {
    cox_bias <- c()
    cox_se <- c()
    cox_mse <- c()
  }
  
  # Prepare S-Lasso and DINA metrics (unpack named list without introducing extra names)
  slasso_bias <- unname(slasso_metrics["bias"])
  slasso_se <- unname(slasso_metrics["se"])
  slasso_mse <- unname(slasso_metrics["mse"])
  
  DINA_bias <- unname(DINA_metrics["bias"])
  DINA_se <- unname(DINA_metrics["se"])
  DINA_mse <- unname(DINA_metrics["mse"])
  
  # Prepare the dataframe for saving
  metrics_df <- data.frame(
    Method = c(rep("Cox", length(cox_metrics_list)), "S-Lasso", "DINA"),
    Specification = c(names(cox_metrics_list), NA, NA),
    Bias = c(cox_bias, slasso_bias, DINA_bias),
    SE = c(cox_se, slasso_se, DINA_se),
    MSE = c(cox_mse, slasso_mse, DINA_mse)
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

# Main function to process results and write metrics to CSV
process_results_to_csv <- function(json_file) {
  # Load experiment configuration
  config <- load_experiment_config(json_file)
  
  # Generate output directory based on the experiment parameters
  output_dir <- generate_output_dir(config, json_file)
  
  # Read the results from the output directory
  results <- read_results(output_dir)
  
  # Extract tau_true
  tau_true <- results$tau_true
  
  # Calculate metrics for Cox, S-Lasso, and DINA methods
  metrics <- calculate_metrics(results, tau_true)
  print(metrics)
  
  # Prepare data for saving to CSV
  metrics_df <- prepare_metrics_dataframe(metrics$cox_metrics_list, metrics$slasso_metrics, metrics$DINA_metrics)
  time_metrics_df <- prepare_time_metrics_dataframe(results)
  
  
  # # Ensure output directory for CSV files
  # json_file_name <- tools::file_path_sans_ext(basename(json_file))  # Extract name without extension
  # output_csv_dir <- ensure_output_csv_dir(json_file_name)
  output_csv_dir <- ensure_output_csv_dir(config, json_file)
  
  # Save metrics and time metrics to CSV files
  save_metrics_to_csv(metrics_df, time_metrics_df, output_csv_dir)
}
