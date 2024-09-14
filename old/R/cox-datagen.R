library(simsurv)
library(dplyr)
library(tidyverse)

generate_simulated_data <- 
  function(is_time_varying, 
         is_linear,
         light_censoring = FALSE){
  
  X <- runif(n, min = -1, max = 1)
  
  if (is_time_varying) {
    A <- rexp(n, rate = exp(alpha * X))
    covariates <- data.frame(id = 1:n, X = X, A = A)
  } else {
    expit <- function(x) 1 / (1 + exp(-x))  # Logistic function
    W <- rbinom(
      n, 1, prob = expit(alpha * X)
    ) 
    covariates <- 
      data.frame(id = 1:n, 
                 X = X, 
                 W = W)
  }
  
  # Generate censoring time C
  if (light_censoring) {
    C <- rep(20, n)  # Set C to a large constant
  } else {
    C <- rexp(n, rate = lambda_C)  # Regular censoring
  }
  
  
  # Define the baseline hazard function
  baseline <- function(t, x, betas, ...) {
    eta_0 <- if (is_linear) {
      log(lambda_0) + 
        betas["X"] * x[, "X"]
    } else {
      log(lambda_0) + 
        betas["X"] * x[, "X"] + 
        betas["X2"] * (x[, "X"]^2)
    }
    
    if (is_time_varying) {
      hazard <- exp(eta_0 + (t >= x[, "A"]) * tau) * (cos(t * 3) + 1) / 2
    } else {
      hazard <- exp(eta_0 + x[, "W"] * tau) * (cos(t * 3) + 1) / 2
    }
    
    return(hazard)
  }
  
  # Define betas with or without the non-linear term
  if (is_linear) {
    betas <- c(X = beta)
  } else {
    betas <- c(X = beta, X2 = beta_2)
  }
  
  # Simulate survival data
  simulated_data <- simsurv(
    hazard = baseline,
    x = covariates,
    interval = c(1e-22, 500),
    betas = betas,  # Coefficient for X
    maxt = 10  # Maximum follow-up time
  )
  
  # Merge covariates with simulated data
  simulated_data <- 
    merge(simulated_data, 
          covariates, 
          by = "id")
  
  # Determine observed times and event indicators
  simulated_data <- simulated_data %>%
    mutate(U = pmin(eventtime, C),
           Delta = as.numeric(eventtime <= C))
  
  # Rename eventtime to T for clarity
  simulated_data <- simulated_data %>%
    rename(T = "eventtime")
  
  # Combine with censoring times
  simulated_data <- cbind(simulated_data, C)
  
  return(simulated_data)
}
