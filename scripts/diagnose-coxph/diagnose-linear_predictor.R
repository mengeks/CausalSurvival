library(here)
source(here("tests/test-helper-cox-loglik.R"))
analyze_survival_distribution <- function(n, p, beta_multiplier, baseline_hazard = 0.5) {
  true_beta <- c(0.5, -0.3, 0.2) * beta_multiplier
  pred_data <- generate_linear_predictor(n, p, true_beta)
  
  survival_times <- rexp(n, rate = baseline_hazard * exp(pred_data$linear_predictor))
  
  results <- list(
    beta_multiplier = beta_multiplier,
    true_beta = true_beta,
    linear_predictor = pred_data$linear_predictor,
    exp_linear_predictor = exp(pred_data$linear_predictor),
    survival_times = survival_times,
    quantiles = list(
      linear_pred = quantile(pred_data$linear_predictor, probs = c(0.25, 0.5, 0.75)),
      exp_linear_pred = quantile(exp(pred_data$linear_predictor), probs = c(0.25, 0.5, 0.75)),
      survival_times = quantile(survival_times, probs = c(0.25, 0.5, 0.75))
    )
  )
  
  return(results)
}

plot_distributions <- function(results, title_prefix = "") {
  par(mfrow = c(2, 2))
  
  hist(results$linear_predictor, 
       main = paste(title_prefix, "Linear Predictor"),
       xlab = "Linear Predictor Value")
  
  hist(results$exp_linear_predictor, 
       main = paste(title_prefix, "exp(Linear Predictor)"),
       xlab = "exp(Linear Predictor) Value")
  
  hist(results$survival_times, 
       main = paste(title_prefix, "Survival Times"),
       xlab = "Survival Time")
  
  par(mfrow = c(1, 1))
}

print_comparison <- function(normal_case, extreme_case) {
  cat("\nComparison of Results:\n")
  cat("====================================\n")
  
  cat("\nBeta Values:\n")
  cat("Normal case (multiplier =", normal_case$beta_multiplier, "):", 
      paste(round(normal_case$true_beta, 3), collapse = ", "), "\n")
  cat("Extreme case (multiplier =", extreme_case$beta_multiplier, "):", 
      paste(round(extreme_case$true_beta, 3), collapse = ", "), "\n")
  
  cat("\n25th Percentile Values:\n")
  cat("Normal case:\n")
  cat(" - Linear Predictor:", round(normal_case$quantiles$linear_pred[1], 3), "\n")
  cat(" - exp(Linear Predictor):", round(normal_case$quantiles$exp_linear_pred[1], 3), "\n")
  cat(" - Survival Times:", round(normal_case$quantiles$survival_times[1], 3), "\n")
  
  cat("\nExtreme case:\n")
  cat(" - Linear Predictor:", round(extreme_case$quantiles$linear_pred[1], 3), "\n")
  cat(" - exp(Linear Predictor):", round(extreme_case$quantiles$exp_linear_pred[1], 3), "\n")
  cat(" - Survival Times:", round(extreme_case$quantiles$survival_times[1], 3), "\n")
}

# Run analysis
set.seed(123)
n <- 1000
p <- 3

# Normal case
normal_case <- analyze_survival_distribution(n, p, beta_multiplier = 1)
plot_distributions(normal_case, "Normal Case: ")

# Extreme case
extreme_case <- analyze_survival_distribution(n, p, beta_multiplier = 8)
plot_distributions(extreme_case, "Extreme Case: ")

# Print comparison
print_comparison(normal_case, extreme_case)
