library(here)
library(dplyr)

# Set parameters
n_list <- c(250, 500, 1000)
R_list <- c(200, 500)
outcomes <- c("linear-interaction", "log")
tau_true <- 1  # The true value of tau

# Function to calculate metrics (Bias, SE, MSE)
calculate_metrics <- function(tau_est, tau_true) {
  bias <- mean(tau_est) - tau_true
  se_est <- sd(tau_est)
  mse <- mean((tau_est - tau_true)^2)
  return(c(bias = bias, se_est = se_est, mse = mse))
}

# Function to process results for a given combination of n, R, and outcome
process_results <- function(n, R, outcome, tau_true) {
  # Read results
  results <- readRDS(here::here(
    "data/outputs/continuous-R-learner",
    paste0("n_", n, "_R_", R, "_outcome_", outcome, ".rds")
  ))
  
  # Calculate metrics for R-Lasso and S-Lasso
  metrics_rlasso <- 
    calculate_metrics(results$tau_est_rlasso, tau_true)
  metrics_slasso <- 
    calculate_metrics(
      results$tau_est_slasso, tau_true)
  
  # Combine metrics into a data frame
  metrics <- data.frame(
    n = n,
    R = R,
    Outcome = outcome,
    Method = c("R-Lasso", "S-Lasso"),
    Bias = c(metrics_rlasso["bias"], metrics_slasso["bias"]),
    SE_Est = c(metrics_rlasso["se_est"], metrics_slasso["se_est"]),
    MSE = c(metrics_rlasso["mse"], metrics_slasso["mse"])
  )
  
  # Calculate time taken for R-Lasso and S-Lasso
  time_metrics <- data.frame(
    n = n,
    R = R,
    Outcome = outcome,
    Method = c("R-Lasso", "S-Lasso"),
    Avg_Time = c(mean(results$time_rlasso), mean(results$time_slasso))
  )
  
  return(list(metrics = metrics, time_metrics = time_metrics))
}

# Initialize lists to store results
all_metrics <- list()
all_time_metrics <- list()

# Loop over n_list, R_list, and outcomes
for (n in n_list) {
  for (R in R_list) {
    for (outcome in outcomes) {
      cat("Processing: n =", n, ", R =", R, ", Outcome =", outcome, "\n")
      
      # Process results for current combination
      res <- process_results(n, R, outcome, tau_true)
      
      # Append to lists
      all_metrics <- append(all_metrics, list(res$metrics))
      all_time_metrics <- append(all_time_metrics, list(res$time_metrics))
    }
  }
}

# Combine all metrics into data frames
metrics_df <- do.call(rbind, all_metrics)
time_metrics_df <- do.call(rbind, all_time_metrics)

# Save metrics and time metrics to CSV
write.csv(
  metrics_df, 
  here::here("tables/continuous-R-learner", "aggregated_metrics.csv"), row.names = FALSE)
write.csv(
  time_metrics_df, 
  here::here("tables/continuous-R-learner", "aggregated_time_metrics.csv"), row.names = FALSE)

cat("Aggregated results saved to CSV files.\n")
