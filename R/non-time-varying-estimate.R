# Load required libraries
library(survival)
library(tidyverse)
# source("R/datagen-helper.R")  # Assuming this function helps load the data
source("R/cox-loglik.R")      # Assuming this has `fit_custom_cox_model`
source("R/lasso.R")

# Function to estimate Cox model (with potential future use of light_censoring)
cox_model_estimation <- function(single_data, model_spec = "correctly-specified", light_censoring = NULL) {
  # Choose the formula based on the specification
  if (model_spec == "correctly-specified") {
    formula <- Surv(U, Delta) ~ W + X.1 + X.2 + X.3 + X.4 + X.5 + X.1:X.2
  } else if (model_spec == "mildly-mis-specified") {
    formula <- Surv(U, Delta) ~ W + X.1 + X.2 + X.3 + X.4 + X.5
  } else if (model_spec == "quite-mis-specified") {
    formula <- Surv(U, Delta) ~ W + X.1 + X.2
  }
  
  # Fit the Cox model based on the chosen formula
  cox_model <- coxph(formula, data = single_data, ties = "breslow")
  
  # Extract the tau estimate (coefficient of W)
  tau_est_cox <- cox_model$coefficients["W"]
  
  return(tau_est_cox)
}

# Function to estimate S-Lasso (with potential future use of light_censoring)
slasso_estimation <- function(single_data, light_censoring = NULL) {
  source("R/lasso.R")
  
  # Fit the S-Lasso model
  slasso_est <- 
    slasso(train_data = single_data, 
           pred_data = NULL)
  
  # Return the tau estimate from S-Lasso
  tau_est_slasso <- slasso_est$tau_hat
  
  return(tau_est_slasso)
}

# Function to estimate nuisance for DINA (uses light_censoring)
DINA_estimate_nuisance <- function(fold_train, fold_test, nuisance_method = "cox", light_censoring) {
  # 1. Estimate the propensity score
  prop_score_model <- glm(W ~ X.2 + X.3, data = fold_train, family = binomial)
  prop_score <- predict(prop_score_model, fold_test, type = "response")
  
  # 2. Calculate a(X) using light_censoring
  if (light_censoring) {
    a_X <- prop_score
  } else {
    p_delta_model <- glm(Delta ~ W + X.1 + X.2 + X.3 + X.4 + X.5, data = fold_train, family = binomial)
    p_delta <- predict(p_delta_model, fold_test, type = "response")
    a_X <- (p_delta * prop_score) / (p_delta * prop_score + (1 - p_delta) * (1 - prop_score))
  }
  
  # 3. Calculate nu(X) based on nuisance method
  if (nuisance_method == "cox") {
    # Use Cox model for nuisance estimation
    cox_model_nu <- coxph(Surv(U, Delta) ~ W + X.1 + X.2 + X.3 + X.4 + X.5 + X.1:X.2, data = fold_train, ties = "breslow")
    coefficients_nu <- coef(cox_model_nu)
    
    # Extract W coefficient and other coefficients
    coeff_W <- coefficients_nu["W"]
    coeff_other <- coefficients_nu[setdiff(names(coefficients_nu), "W")]
    
    # Create the design matrix for fold_test (without W)
    X_fold_test <- model.matrix(~ X.1 + X.2 + X.3 + X.4 + X.5 + X.1 * X.2, data = fold_test)[, -1]
    
    # Compute eta_0 and eta_1
    eta_0 <- as.vector(X_fold_test %*% coeff_other)
    eta_1 <- eta_0 + coeff_W
    
  } else if (nuisance_method == "slasso") {
    # Use S-Lasso for nuisance estimation
    slasso_est <- slasso(train_data = fold_train, pred_data = fold_test)
    tau_estimate <- slasso_est$tau_hat
    eta_0 <- slasso_est$eta_0_hat
    eta_1 <- eta_0 + tau_estimate
  }
  
  # 4. Combine a(X) and nu(X)
  nu_X <- a_X * eta_1 + (1 - a_X) * eta_0
  fold_test$a_X <- a_X
  fold_test$nu_X <- nu_X
  
  return(fold_test)
}

# Helper function to fit final model (DINA estimation)
fit_final_model <- function(fold_nuisance, final_model_method) {
  if (final_model_method == "custom_cox") {
    covar_fold <- matrix(fold_nuisance$W - fold_nuisance$a_X, ncol = 1)
    final_model <- fit_custom_cox_model(
      init_beta = 0,
      time = fold_nuisance$U,
      status = fold_nuisance$Delta,
      covar = covar_fold,
      strata = rep(1, length(fold_nuisance$U)),
      offset = fold_nuisance$nu_X
    )
    tau_est <- final_model$par
  } else if (final_model_method == "coxph") {
    final_model <- coxph(Surv(U, Delta) ~ I(W - a_X) + offset(nu_X), data = fold_nuisance, ties = "breslow")
    tau_est <- summary(final_model)$coefficients[1]
  }
  return(tau_est)
}

# # Function to estimate DINA (uses light_censoring)
# DINA_estimation <- function(fold1, fold2, nuisance_method = "cox", final_model_method = "custom_cox", light_censoring) {
#   # Estimate nuisance for fold2 using fold1
#   fold2_nuisance <- DINA_estimate_nuisance(fold1, fold2, nuisance_method = nuisance_method, light_censoring = light_censoring)
#   
#   # Estimate nuisance for fold1 using fold2
#   fold1_nuisance <- DINA_estimate_nuisance(fold2, fold1, nuisance_method = nuisance_method, light_censoring = light_censoring)
#   
#   # Function to fit final model and estimate tau for one fold
#   fit_final_model <- function(fold_nuisance, final_model_method) {
#     if (final_model_method == "custom_cox") {
#       covar_fold <- matrix(fold_nuisance$W - fold_nuisance$a_X, ncol = 1)
#       final_model <- fit_custom_cox_model(
#         init_beta = 0,
#         time = fold_nuisance$U,
#         status = fold_nuisance$Delta,
#         covar = covar_fold,
#         strata = rep(1, length(fold_nuisance$U)),
#         offset = fold_nuisance$nu_X
#       )
#       tau_est <- final_model$par
#     } else if (final_model_method == "coxph") {
#       final_model <- coxph(Surv(U, Delta) ~ I(W - a_X) + offset(nu_X), data = fold_nuisance, ties = "breslow")
#       tau_est <- summary(final_model)$coefficients[1]
#     }
#     return(tau_est)
#   }
#   
#   # Estimate tau for fold2 using fold1's nuisance estimates
#   tau_est_fold2 <- fit_final_model(fold2_nuisance, final_model_method)
#   
#   # Estimate tau for fold1 using fold2's nuisance estimates
#   tau_est_fold1 <- fit_final_model(fold1_nuisance, final_model_method)
#   
#   # Take the average of the two tau estimates
#   tau_est_DINA <- (tau_est_fold1 + tau_est_fold2) / 2
#   
#   return(tau_est_DINA)
# }
