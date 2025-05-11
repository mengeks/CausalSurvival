#' @title Time-Varying Causal Survival Learning (TV-CSL): Final Model Fitting
#' @description Functions for fitting the final causal model and making predictions.
#'
#' @importFrom survival coxph Surv
#' @importFrom glmnet cv.glmnet
#' @importFrom stats as.formula coef

#' Fit the final TV-CSL model
#'
#' @description Fits the final model to estimate treatment effects
#' using orthogonalized scores.
#'
#' @param data Data frame containing the data for fitting
#' @param propensity_scores Propensity scores for each observation
#' @param nuisance_estimates Nuisance parameter estimates from estimate_nuisance_parameters()
#' @param treatment_effect_form Form of the treatment effect (default: "constant")
#' @param covariates Names of covariates to use (default: NULL, uses all numeric columns)
#' @param treatment_indicator Column name for treatment indicator (default: "W")
#' @param tstart Column name for interval start time (default: "tstart")
#' @param tstop Column name for interval end time (default: "tstop")
#' @param event_indicator Column name for event indicator (default: "Delta")
#' @param method Method for fitting the final model (default: "coxph")
#' @param lambda_seq Lambda sequence for lasso-coxph (default: NULL = auto)
#'
#' @return A list containing the fitted model and coefficient estimates
#'
#' @keywords internal
fit_final_model <- function(data,
                           propensity_scores,
                           nuisance_estimates,
                           treatment_effect_form = "constant",
                           covariates = NULL,
                           treatment_indicator = "W",
                           tstart = "tstart",
                           tstop = "tstop",
                           event_indicator = "Delta",
                           method = "coxph",
                           lambda_seq = NULL) {
  
  # Select covariates if not specified
  if (is.null(covariates)) {
    # Use all numeric columns as covariates, excluding special columns
    numeric_cols <- sapply(data, is.numeric)
    excluded_cols <- c(treatment_indicator, tstart, tstop, event_indicator, "id")
    numeric_cols[excluded_cols[excluded_cols %in% names(numeric_cols)]] <- FALSE
    covariates <- names(data)[numeric_cols]
  }
  
  # Calculate nu (the offset term)
  data$nu <- propensity_scores * nuisance_estimates$y1_pred + 
            (1 - propensity_scores) * nuisance_estimates$y0_pred
  
  # Calculate treatment indicator minus propensity score
  # (This is used for orthogonalization)
  data$trt_minus_ps <- data[[treatment_indicator]] - propensity_scores
  
  # Prepare data for final model based on treatment effect form
  if (treatment_effect_form == "constant") {
    # Only a constant treatment effect
    interaction_terms <- "trt_minus_ps"
    data$intercept_term <- data$trt_minus_ps
  } else if (treatment_effect_form == "linear") {
    # Treatment effect varies with covariates
    interaction_terms <- c("trt_minus_ps")
    data$intercept_term <- data$trt_minus_ps
    
    # Add interactions for each covariate
    for (cov in covariates) {
      term_name <- paste0("trt_minus_ps_by_", cov)
      data[[term_name]] <- data$trt_minus_ps * data[[cov]]
      interaction_terms <- c(interaction_terms, term_name)
    }
  } else {
    stop("Invalid treatment_effect_form. Choose 'constant' or 'linear'.")
  }
  
  # Fit final model based on specified method
  if (method == "coxph") {
    # Standard Cox proportional hazards model
    formula_str <- paste0("Surv(", tstart, ", ", tstop, ", ", event_indicator, 
                         ") ~ ", paste(interaction_terms, collapse = " + "), 
                         " + offset(nu)")
    
    model <- coxph(as.formula(formula_str), data = data, ties = "breslow")
    coefficients <- coef(model)
  } else if (method == "lasso-coxph") {
    # Lasso-penalized Cox model
    X_final <- as.matrix(data[, interaction_terms, drop = FALSE])
    
    if (is.null(lambda_seq)) {
      # Use default lambda sequence
      model <- cv.glmnet(X_final, 
                        Surv(data[[tstart]], data[[tstop]], data[[event_indicator]]),
                        offset = data$nu,
                        family = "cox",
                        nlambda = 50,
                        nfolds = 5)
    } else {
      # Use specified lambda sequence
      model <- cv.glmnet(X_final, 
                        Surv(data[[tstart]], data[[tstop]], data[[event_indicator]]),
                        offset = data$nu,
                        family = "cox",
                        lambda = lambda_seq,
                        nlambda = length(lambda_seq),
                        nfolds = 5)
    }
    
    coefficients <- as.vector(coef(model, s = "lambda.min"))
  } else {
    stop("Invalid method. Choose 'coxph' or 'lasso-coxph'.")
  }
  
  # Return model and coefficients
  return(list(
    model = model,
    coefficients = coefficients,
    treatment_effect_form = treatment_effect_form,
    method = method,
    interaction_terms = interaction_terms
  ))
}

#' Predict treatment effects using the fitted TV-CSL model
#'
#' @description Makes predictions for the treatment effect for new data
#'
#' @param model A fitted TV-CSL model from fit_final_model()
#' @param newdata Data frame containing new data for prediction
#' @param covariates Names of covariates used in the model
#'
#' @return A vector of predicted treatment effects
#'
#' @keywords internal
predict_treatment_effects <- function(model,
                                     newdata,
                                     covariates = NULL) {
  
  # Extract model components
  coefficients <- model$coefficients
  treatment_effect_form <- model$treatment_effect_form
  interaction_terms <- model$interaction_terms
  
  # Select covariates if not specified
  if (is.null(covariates) && treatment_effect_form == "linear") {
    # Try to infer covariates from interaction terms
    cov_terms <- interaction_terms[grep("trt_minus_ps_by_", interaction_terms)]
    covariates <- sub("trt_minus_ps_by_", "", cov_terms)
  }
  
  # Create design matrix for predictions
  if (treatment_effect_form == "constant") {
    # Only intercept term
    X_pred <- matrix(1, nrow = nrow(newdata), ncol = 1)
  } else if (treatment_effect_form == "linear") {
    # Intercept plus covariates
    X_pred <- cbind(1, as.matrix(newdata[, covariates, drop = FALSE]))
  }
  
  # Calculate predictions
  predictions <- X_pred %*% coefficients
  
  return(predictions)
}