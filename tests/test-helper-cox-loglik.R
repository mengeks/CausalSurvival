library(here)
library(survival)
source(here("R/cox-loglik.R"))

generate_linear_predictor <- function(n, p, true_beta) {
  covar <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(covar) <- paste0("covar.", 1:p)
  linear_predictor <- covar %*% true_beta
  return(list(linear_predictor = linear_predictor, covar = covar))
}

generate_cox_data <- function(n, p, true_beta, baseline_hazard = 0.1, event_prob = 0.7, offset = NULL) {
  pred_data <- generate_linear_predictor(n, p, true_beta)
  
  if (is.null(offset)) {
    offset <- rep(0, n)
  }
  
  time <- rexp(n, rate = baseline_hazard * exp(pred_data$linear_predictor + offset))
  status <- rbinom(n, 1, event_prob)
  strata <- rep(1, n)
  
  return(list(
    time = time,
    status = status,
    covar = pred_data$covar,
    strata = strata,
    offset = offset
  ))
}

# Step 3: Perform Comparison with coxph
compare_with_coxph <- function(time, status, covar, offset = NULL) {
  # Convert to data frame for coxph
  data <- 
    data.frame(time = time, status = status, covar)
  
  # Fit the Cox model using coxph, dynamically generating the formula based on the number of covariates
  formula_string <- paste("Surv(time, status) ~", 
                          paste(colnames(covar), collapse = " + "))
  formula <- as.formula(formula_string)
  
  # If offset is provided, include it in the coxph call
  if (!is.null(offset)) {
    data$offset <- offset  # Add the offset to the data frame
    coxph_fit <- coxph(formula, data = data, offset = offset)
  } else {
    coxph_fit <- coxph(formula, data = data)
  }
  
  # Extract the estimated coefficients
  coxph_coefs <- coef(coxph_fit)
  
  # Return the coxph model object and coefficients
  return(list(model = coxph_fit, coefs = coxph_coefs))
}


# Step 4: Test Function to Run Everything and Output the Results
test_cox_loglik <- function(n, p, true_beta, init_beta, offset = NULL, seed_value = 123) {
  
  data <- generate_cox_data(n = n, p = p, true_beta = true_beta, offset = offset)
  
  # Fit the model using custom Cox log-likelihood
  result <- fit_custom_cox_model(init_beta = init_beta, 
                                 time = data$time, 
                                 status = data$status, 
                                 covar = data$covar, 
                                 strata = data$strata, 
                                 offset = data$offset)
  
  # Print the true beta values
  print("True beta values:")
  print(true_beta)
  
  # Print the optimized beta estimates
  print("Optimized beta estimates from custom model:")
  print(result$par)
  
  # Compare with true beta
  beta_diff <- result$par - true_beta
  print("Difference between estimated beta and true beta:")
  print(beta_diff)
  
  # Compare with coxph
  coxph_result <- compare_with_coxph(time = data$time, status = data$status, covar = data$covar)
  
  # Print the coxph coefficients
  print("Beta estimates from coxph model:")
  print(coxph_result$coefs)
  
  # Compare custom model beta with coxph beta
  coxph_diff <- result$par - coxph_result$coefs
  print("Difference between custom model beta and coxph beta:")
  print(coxph_diff)
  
  # Print the log-likelihood at the optimum
  print("Log-likelihood at the optimum (custom model):")
  print(-result$value)
}
