# Cox PH Likelihood Convexity Demonstration
library(survival)

# Function to calculate the partial log-likelihood for a given beta
cox_log_likelihood <- function(beta, Z, time, status) {
  n <- length(time)
  events <- which(status == 1)
  log_lik <- 0
  
  for(i in events) {
    risk_set <- which(time >= time[i])
    log_lik <- log_lik + beta * Z[i] - log(sum(exp(beta * Z[risk_set])))
  }
  
  return(log_lik)
}

# Simulation settings
set.seed(123)
n <- 200

# Generate data under different scenarios
simulate_and_plot <- function(Z_dist, lambda_type, title) {
  # Generate covariates
  if(Z_dist == "normal") {
    Z <- rnorm(n)
  } else if(Z_dist == "binary") {
    Z <- rbinom(n, 1, 0.5)
  } else if(Z_dist == "skewed") {
    Z <- rexp(n) - 1
  }
  
  # True beta
  true_beta <- 0.8
  
  # Generate survival times
  if(lambda_type == "constant") {
    lambda0 <- 0.1
    lambda <- lambda0 * exp(true_beta * Z)
    time <- rexp(n, lambda)
  } else if(lambda_type == "weibull") {
    shape <- 1.5
    scale <- 10
    u <- runif(n)
    time <- scale * (-log(u) * exp(-true_beta * Z))^(1/shape)
  }
  
  # Generate censoring
  cens_time <- runif(n, 0, max(time) * 1.5)
  obs_time <- pmin(time, cens_time)
  status <- as.numeric(time <= cens_time)
  
  # Ensure we have events
  if(sum(status) < 10) {
    cens_time <- rexp(n, 0.05)
    obs_time <- pmin(time, cens_time)
    status <- as.numeric(time <= cens_time)
  }
  
  # Calculate log-likelihood for a range of beta values
  beta_range <- seq(-2, 3, by=0.1)
  log_lik <- sapply(beta_range, cox_log_likelihood, Z=Z, time=obs_time, status=status)
  
  # Fit Cox model to find MLE
  fit <- coxph(Surv(obs_time, status) ~ Z)
  est_beta <- coef(fit)
  
  # Plot
  plot(beta_range, log_lik, type="l", lwd=2, col="blue", 
       xlab="Beta", ylab="Log-Partial Likelihood",
       main=paste("Cox PH Log-Likelihood -", title))
  abline(v=est_beta, col="red", lty=2)
  abline(v=true_beta, col="green", lty=2)
  legend("bottomright", legend=c("Log-Likelihood", "Estimated Beta", "True Beta"),
         col=c("blue", "red", "green"), lty=c(1, 2, 2), lwd=c(2, 1, 1))
}

# Create plots with different distributions
par(mfrow=c(2,3), mar=c(4,4,3,1))

# Different covariate distributions
simulate_and_plot("normal", "constant", "Normal Z, Constant Baseline")
simulate_and_plot("binary", "constant", "Binary Z, Constant Baseline")
simulate_and_plot("skewed", "constant", "Skewed Z, Constant Baseline")

# Different baseline hazards
simulate_and_plot("normal", "weibull", "Normal Z, Weibull Baseline")
simulate_and_plot("binary", "weibull", "Binary Z, Weibull Baseline")
simulate_and_plot("skewed", "weibull", "Skewed Z, Weibull Baseline")

# Check convexity numerically
check_convexity <- function() {
  Z <- rnorm(n)
  true_beta <- 0.8
  lambda0 <- 0.1
  lambda <- lambda0 * exp(true_beta * Z)
  time <- rexp(n, lambda)
  cens_time <- runif(n, 0, max(time) * 1.5)
  obs_time <- pmin(time, cens_time)
  status <- as.numeric(time <= cens_time)
  
  beta_values <- seq(-2, 3, by=0.05)
  log_lik <- sapply(beta_values, cox_log_likelihood, Z=Z, time=obs_time, status=status)
  
  # Calculate numerical second derivative
  second_deriv <- diff(diff(log_lik)) / 0.05^2
  
  # Check if all second derivatives are non-positive (allowing for numerical error)
  all_concave <- all(second_deriv < 1e-10)
  
  cat("Is log-likelihood concave? ", all_concave, "\n")
  cat("Range of second derivatives: [", min(second_deriv), ", ", max(second_deriv), "]\n")
}

check_convexity()