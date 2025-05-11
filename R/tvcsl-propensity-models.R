#' @title Time-Varying Causal Survival Learning (TV-CSL): Propensity Score Estimation
#' @description Functions for estimating propensity scores in time-varying treatment settings.
#'
#' @importFrom survival coxph Surv
#' @importFrom dplyr mutate filter select %>%
#' @importFrom stats as.formula coef

#' Estimate propensity scores for treatment adoption
#'
#' @description Estimates the probability of receiving treatment by time t
#' conditional on covariates, using a Cox proportional hazards model.
#'
#' @param data A data frame containing the covariates and treatment times
#' @param covariates Names of covariate columns to use in the model (default: NULL, uses all numeric columns)
#' @param treatment_time Column name for time to treatment (default: "A")
#' @param event_time Column name for time to event (default: "U")
#' @param event_indicator Column name for event indicator (default: "Delta")
#' @param model_type Type of model to use (default: "cox-linear")
#'   - "cox-linear": Cox model with linear terms
#'   - "cox-nonlinear": Cox model with nonlinear transformations
#' @param use_censored Whether to use censored observations (default: TRUE)
#'
#' @return A list containing the model and coefficient estimates
#'
#' @keywords internal
estimate_propensity_scores <- function(data,
                                      covariates = NULL,
                                      treatment_time = "A",
                                      event_time = "U",
                                      event_indicator = "Delta",
                                      model_type = "cox-linear",
                                      use_censored = TRUE) {
  
  # Prepare data for propensity estimation
  data <- data %>%
    mutate(treatment_time_for_ps = ifelse(is.na(!!sym(treatment_time)), Inf, !!sym(treatment_time)),
           min_time = pmin(!!sym(event_time), treatment_time_for_ps),
           treatment_event = !is.infinite(treatment_time_for_ps) & 
                           treatment_time_for_ps <= !!sym(event_time))
  
  # Filter data if only using non-censored observations
  if (!use_censored) {
    data <- data %>% filter(!!sym(event_indicator) == 1)
  }
  
  # Select covariates if not specified
  if (is.null(covariates)) {
    # Use all numeric columns as covariates
    numeric_cols <- sapply(data, is.numeric)
    numeric_cols[c("treatment_time_for_ps", "min_time", "treatment_event")] <- FALSE
    covariates <- names(data)[numeric_cols]
  }
  
  # Create covariate formula for model
  if (model_type == "cox-linear") {
    covariate_terms <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste("Surv(min_time, treatment_event) ~", covariate_terms))
  } else if (model_type == "cox-nonlinear") {
    # Transform covariates
    X_nonlinear <- transform_covariates(data[, covariates, drop = FALSE], 
                                        transform_type = "nonlinear")
    data_nonlinear <- cbind(data, X_nonlinear)
    nonlinear_terms <- paste(colnames(X_nonlinear), collapse = " + ")
    formula <- as.formula(paste("Surv(min_time, treatment_event) ~", nonlinear_terms))
  } else {
    stop("Invalid model_type. Choose 'cox-linear' or 'cox-nonlinear'.")
  }
  
  # Fit the Cox model
  treatment_model <- coxph(formula, data = data, ties = "breslow")
  
  # Return model and coefficients
  return(list(
    model = treatment_model,
    coefficients = coef(treatment_model),
    formula = formula,
    covariates = covariates,
    model_type = model_type
  ))
}

#' Calculate propensity scores for each time point
#'
#' @description Calculates the probability of receiving treatment by time t
#' for each observation, based on a fitted propensity score model.
#'
#' @param data A data frame containing data for which to calculate propensity scores
#' @param ps_model A propensity score model fitted by estimate_propensity_scores()
#' @param time_points Vector of time points at which to calculate propensity scores
#'
#' @return A matrix of propensity scores, with rows for observations and columns for time points
#'
#' @keywords internal
calculate_propensity_scores <- function(data,
                                       ps_model,
                                       time_points) {
  
  # Extract components from the model
  coef_estimates <- ps_model$coefficients
  model_type <- ps_model$model_type
  covariates <- ps_model$covariates
  
  # Prepare covariates based on model type
  if (model_type == "cox-linear") {
    X_data <- as.matrix(data[, covariates, drop = FALSE])
  } else if (model_type == "cox-nonlinear") {
    X_data <- transform_covariates(data[, covariates, drop = FALSE], 
                                  transform_type = "nonlinear")
  }
  
  # Calculate linear predictor
  linear_predictor <- X_data %*% coef_estimates
  
  # Calculate hazard at each time point
  hazard_adjust <- exp(linear_predictor)
  
  # Initialize propensity score matrix
  ps_matrix <- matrix(0, nrow = nrow(data), ncol = length(time_points))
  
  # Calculate propensity scores for each time point
  for (i in seq_along(time_points)) {
    t <- time_points[i]
    ps_matrix[, i] <- 1 - exp(-t * hazard_adjust)
  }
  
  # Add column names
  colnames(ps_matrix) <- paste0("ps_t", time_points)
  
  return(ps_matrix)
}