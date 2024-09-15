# Function to process results and write metrics to CSV
process_results_to_csv <- function(json_file) {
  
  # Load experiment output configuration (JSON)
  config <- fromJSON(json_file)
  
  # Extract parameters from JSON for directory
  n <- config$experiment$n
  R <- config$experiment$R
  
  # Generate the output directory based on the input parameters
  json_file_name <- tools::file_path_sans_ext(basename(json_file))  # Get the name of the JSON file without the extension
  output_dir <- paste0("data/outputs/replicate-DINA/n_", n, "_R_", R, "/", json_file_name)
  
  # Read the results
  results_file <- here::here(output_dir, "results.rds")
  results <- readRDS(file = results_file)
  
  # Extract tau_true
  tau_true <- results$tau_true
  
  # Calculate summary metrics for each method
  if (length(results$tau_estimates_cox) > 0) {
    cox_metrics_list <- lapply(results$tau_estimates_cox, calculate_bias_se_mse, tau_true = tau_true)
  } else {
    cox_metrics_list <- list()  # Empty list if no Cox results
  }
  
  # Check and process S-Lasso results
  if (length(results$tau_estimates_slasso) > 0) {
    slasso_metrics <- calculate_bias_se_mse(results$tau_estimates_slasso, tau_true)
  } else {
    slasso_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA or empty if no S-Lasso results
  }
  # Check and process DINA results
  if (length(results$tau_estimates_DINA) > 0) {
    # Access the sublist inside `results$tau_estimates_DINA` (e.g., "cox_custom_cox")
    DINA_estimates <- results$tau_estimates_DINA$cox_custom_cox
    
    # Calculate bias, SE, MSE for DINA
    DINA_metrics <- calculate_bias_se_mse(DINA_estimates, tau_true)
  } else {
    DINA_metrics <- list(bias = NA, se = NA, mse = NA)  # Set to NA or empty if no DINA results
  }
  
  print(DINA_metrics)
  
  
  # Prepare data for saving
  metrics_df <- data.frame(
    Method = c(rep("Cox", length(cox_metrics_list)), "S-Lasso", "DINA"),
    Specification = c(names(cox_metrics_list), NA, NA),
    Bias = c(sapply(cox_metrics_list, `[`, "bias"), slasso_metrics["bias"], DINA_metrics["bias"]),
    SE = c(sapply(cox_metrics_list, `[`, "se"), slasso_metrics["se"], DINA_metrics["se"]),
    MSE = c(sapply(cox_metrics_list, `[`, "mse"), slasso_metrics["mse"], DINA_metrics["mse"])
  )
  
  # Time taken summary for each method
  cox_time_list <- sapply(results$time_taken$cox, mean)
  slasso_time <- mean(results$time_taken$slasso)
  DINA_time <- mean(results$time_taken$DINA)
  
  # Prepare time data for saving
  time_metrics_df <- data.frame(
    Method = c(rep("Cox", length(cox_time_list)), "S-Lasso", "DINA"),
    Specification = c(names(cox_time_list), NA, NA),
    Avg_Time = c(cox_time_list, slasso_time, DINA_time)
  )
  
  # Ensure output directory for CSVs
  output_csv_dir <- here::here("tables/replicate-DINA", json_file_name)
  dir.create(output_csv_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save the results to CSV
  metrics_csv_file <- file.path(output_csv_dir, "aggregated_metrics.csv")
  write.csv(metrics_df, metrics_csv_file, row.names = FALSE)
  
  time_metrics_csv_file <- file.path(output_csv_dir, "aggregated_time_metrics.csv")
  write.csv(time_metrics_df, time_metrics_csv_file, row.names = FALSE)
  
  # Output completion message with directory info
  cat("Aggregated results saved to CSV files in:", output_csv_dir, "\n")
}