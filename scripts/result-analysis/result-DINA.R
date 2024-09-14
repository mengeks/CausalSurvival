library(jsonlite)
library(here)
library(dplyr)
source("R/result-summaries.R")

# Load experiment output configuration (JSON)
config <- fromJSON("scripts/result-analysis/output-DINA.json")

# Read the results
results <- readRDS(
  file = here::here("data/outputs/non-time-varying-cox", 
                    paste0("n_", config$experiment$n, "_R_", config$experiment$R, "/results.rds"))
)

# Extract tau_true
tau_true <- results$tau_true

# Calculate summary metrics for each method
cox_metrics_list <- lapply(results$tau_estimates_cox, calculate_bias_se_mse, tau_true = tau_true)
slasso_metrics <- calculate_bias_se_mse(results$tau_estimates_slasso, tau_true)
DINA_metrics <- calculate_bias_se_mse(results$tau_estimates_DINA, tau_true)

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

# Save the results to CSV
write.csv(
  metrics_df, 
  here::here("tables/non-time-varying-cox", "aggregated_metrics.csv"), 
  row.names = FALSE
)

write.csv(
  time_metrics_df, 
  here::here("tables/non-time-varying-cox", "aggregated_time_metrics.csv"), 
  row.names = FALSE
)

cat("Aggregated results saved to CSV files.\n")
