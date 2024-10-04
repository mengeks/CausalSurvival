
library(survival)
library(tidyverse)
source("R/cox-loglik.R")

#' Create Pseudo Dataset for Time-Varying Cox Model
#'
#' This function transforms the original survival dataset into a pseudo dataset 
#' required for fitting a time-varying Cox proportional hazards model. The transformation 
#' is based on whether the event time (\code{U}) is before or after the intervention time (\code{A}).
#' 
#' @param survival_data A data frame containing survival data with columns: 
#' \code{U} (event time), \code{Delta} (event indicator), \code{A} (intervention time), 
#' \code{id} (subject ID), and other covariates.
#' @return A pseudo dataset suitable for fitting a time-varying Cox model, containing 
#' columns: \code{tstart}, \code{tstop}, \code{Delta}, \code{W} (indicator for time-varying covariate),
#' and any additional covariates from the original \code{survival_data}.
#' @examples
#' survival_data <- data.frame(U = c(5, 3), Delta = c(1, 0), A = c(2, 4), id = 1:2, X1 = c(1, 2), X2 = c(0.5, 1.5))
#' create_pseudo_dataset(survival_data)
#' 
#' @export
create_pseudo_dataset <- function(survival_data) {
  # Initialize an empty tibble for the pseudo dataset
  pseudo_dataset <- tibble(
    tstart = numeric(),
    tstop = numeric(),
    Delta = numeric(),
    W = numeric()
  )
  
  # Columns to retain in the pseudo dataset (excluding U, Delta, A, and id)
  covariates <- setdiff(colnames(survival_data), c("U", "Delta", "A", "id"))
  
  # Iterate over each row in the survival_data
  for (i in 1:nrow(survival_data)) {
    U_i <- survival_data$U[i]
    Delta_i <- survival_data$Delta[i]
    A_i <- survival_data$A[i]
    id_i <- survival_data$id[i]
    
    # Covariates to retain for each individual
    covariate_values <- survival_data[i, covariates, drop = FALSE]
    
    if (U_i <= A_i && A_i <= Inf) {
      # One pseudo observation
      new_rows <- tibble(
        tstart = 0,
        tstop = U_i,
        Delta = Delta_i,
        W = 0
      )
    } else if (A_i < U_i) {
      # Two pseudo observations
      new_rows <- tibble(
        tstart = c(0, A_i),
        tstop = c(A_i, U_i),
        Delta = c(0, Delta_i),
        W = c(0, 1)
      )
    }
    
    # Add ID and covariates to the new rows
    new_rows <- new_rows %>% 
      mutate(id = id_i) %>%
      bind_cols(covariate_values)
    
    # Append the new rows to the pseudo dataset
    pseudo_dataset <- bind_rows(pseudo_dataset, new_rows)
  }
  
  # Remove any rare cases where time might be zero or negative
  pseudo_dataset <- pseudo_dataset %>% 
    mutate(t = tstop - tstart) %>%
    filter(t > 0.001) %>%
    select(-t)
  
  return(pseudo_dataset)
}


#' Generate the regressor part of the Cox model formula
#'
#' @param model_spec Character string specifying the model. Options are:
#' \itemize{
#'   \item "correctly-specified"
#'   \item "mildly-mis-specified"
#'   \item "quite-mis-specified"
#' }
#' @return A character string representing the regressor part of the formula.
#' @export
generate_regressor_part <- function(model_spec = "correctly-specified", 
                                    CATE_type = "constant", 
                                    eta_type = "linear-interaction") {
  
  # Handle W based on CATE_type
  if (CATE_type == "constant") {
    W_part <- "W"
  } else if (CATE_type == "linear") {
    # W_part <- "W * (X.1 + X.10)"
    W_part <- "W:X.1 + W:X.10"
  }
  
  if (model_spec == "correctly-specified") {
    if (eta_type == "10-dim-non-linear") {
      return(paste(W_part, "+ sqrt(abs(X.1 * X.2)) + sqrt(abs(X.10)) + cos(X.5) + cos(X.5) * cos(X.6)"))
    } else {
      return(paste(W_part, "+ X.1 + X.2 + X.3 + X.4 + X.5 + X.6 + X.7 + X.8"))
    }
  } else if (model_spec == "mildly-mis-specified") {
    return(paste(W_part, "+ X.1 + X.2 + X.3 + X.4 + X.5"))
  } else if (model_spec == "quite-mis-specified") {
    return(paste(W_part, "+ X.1 + X.2"))
  }
}


#' Create the Cox model formula
#'
#' @param model_spec Character string specifying the model. Options are:
#' \itemize{
#'   \item "correctly-specified"
#'   \item "mildly-mis-specified"
#'   \item "quite-mis-specified"
#' }
#' @param run_time_varying Logical value. If TRUE, the time-varying Cox model is used.
#' @return A formula object for the Cox model.
#' @export
create_cox_formula <- function(model_spec, 
                               run_time_varying,
                               CATE_type = "constant",
                               eta_type = "linear-interaction") {
  regressor_part <- 
    generate_regressor_part(model_spec = model_spec,
                            CATE_type = CATE_type,
                            eta_type = eta_type)
  
  if (run_time_varying) {
    formula <- as.formula(paste("Surv(tstart, tstop, Delta) ~", regressor_part))
  } else {
    formula <- as.formula(paste("Surv(U, Delta) ~", regressor_part))
  }
  
  return(formula)
}

#' Preprocess data for Cox model based on time-varying flag
#'
#' @param single_data The input dataset.
#' @param run_time_varying Logical value. If TRUE, the time-varying Cox model is used.
#' @return Preprocessed dataset.
#' @export
preprocess_data <- function(single_data, run_time_varying) {
  
  if (run_time_varying) {
    return(create_pseudo_dataset(survival_data = single_data))
  } else {
    return(single_data %>% mutate(W = as.numeric(A <= U)))
  }
}

#' Estimate Cox model
#'
#' @param single_data The input dataset.
#' @param methods_cox A list containing model specification and run_time_varying flag.
#' @param light_censoring Optional parameter for light censoring.
#' @return The estimated coefficient for W in the Cox model.
#' @export
cox_model_estimation <- function(
    single_data, 
    methods_cox, 
    light_censoring = NULL,
    CATE_type = "constant",
    eta_type = "linear-interaction"
  ) {
  model_spec <- methods_cox$model_spec
  run_time_varying <- methods_cox$run_time_varying
  
  data_to_use <- 
    preprocess_data(single_data, 
                    run_time_varying)
  
  formula <- 
    create_cox_formula(model_spec = model_spec, 
                       run_time_varying = run_time_varying,
                       CATE_type = CATE_type,
                       eta_type = eta_type)
  
  cox_model <- 
    coxph(formula, 
          data = data_to_use, 
          ties = "breslow")
  
  # tau_est_cox <- cox_model$coefficients["W"]
  beta_est_cox <- extract_W_coefficients(
    fit=cox_model, 
    CATE_type = "constant")
  
  # return(tau_est_cox)
  return(beta_est_cox)
}


extract_W_coefficients <- function(fit, CATE_type = "constant") {
  all_coefs <- coef(fit)
  
  if (CATE_type == "constant") {
    W_part <- "W"
  } else if (CATE_type == "linear") {
    W_part <- "W:X.1|W:X.10"  # Modify as per how W interacts with X covariates in your model
  } else {
    stop("Unsupported CATE_type")
  }
  
  W_coefs <- all_coefs[grep(W_part, names(all_coefs))]
  
  return(W_coefs)
}

