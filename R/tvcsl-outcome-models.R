#' @title Time-Varying Causal Survival Learning (TV-CSL): Outcome Model Estimation
#' @description Functions for estimating nuisance parameters and outcome models.
#'
#' @importFrom survival Surv
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom stats coef predict

#' Estimate nuisance parameters for TV-CSL
#'
#' @description Fits models for the control and treated hazards to estimate
#' nuisance parameters needed for the final model.
#'
#' @param train_data Training data for fitting models
#' @param test_data Test data for which to make predictions
#' @param outcome_model Type of outcome model to use (default: "s-learner")
#'   - "t-learner": Separate models for treated and control
#'   - "s-learner": Single model with treatment as a feature
#'   - "m-learner": Marginal model ignoring treatment
#' @param transform_type Type of covariate transformation (default: "linear")
#' @param covariates Names of covariates to use (default: NULL, uses all numeric columns)
#' @param treatment_indicator Column name for treatment indicator (default: "W")
#' @param tstart Column name for interval start time (default: "tstart")
#' @param tstop Column name for interval end time (default: "tstop")
#' @param event_indicator Column name for event indicator (default: "Delta")
#' @param lambda_seq Lambda sequence for lasso (default: NULL = auto)
#' @param nlambda Number of lambda values (default: 50)
#' @param fast_cv Whether to use a faster CV for lasso (default: TRUE)
#'
#' @return A list containing fitted models and predictions
#'
#' @keywords internal
estimate_nuisance_parameters <- function(train_data,
                                        test_data,
                                        outcome_model = "s-learner",
                                        transform_type = "linear",
                                        covariates = NULL,
                                        treatment_indicator = "W",
                                        tstart = "tstart",
                                        tstop = "tstop",
                                        event_indicator = "Delta",
                                        lambda_seq = NULL,
                                        nlambda = 50,
                                        fast_cv = TRUE) {
  
  # Select covariates if not specified
  if (is.null(covariates)) {
    # Use all numeric columns as covariates, excluding special columns
    numeric_cols <- sapply(train_data, is.numeric)
    excluded_cols <- c(treatment_indicator, tstart, tstop, event_indicator, "id")
    numeric_cols[excluded_cols[excluded_cols %in% names(numeric_cols)]] <- FALSE
    covariates <- names(train_data)[numeric_cols]
  }
  
  # Transform covariates
  X_train <- transform_covariates(train_data[, covariates, drop = FALSE], 
                                 transform_type = transform_type)
  X_test <- transform_covariates(test_data[, covariates, drop = FALSE], 
                                transform_type = transform_type)
  
  # Create survival object for Cox models
  surv_train <- Surv(train_data[[tstart]], train_data[[tstop]], train_data[[event_indicator]])
  
  # Configure lambda sequence for faster CV if requested
  if (fast_cv) {
    if (is.null(lambda_seq)) {
      # Generate a smaller lambda sequence for faster CV
      lambda_seq <- exp(seq(log(0.1), log(10), length.out = nlambda))
    }
    cv_args <- list(lambda = lambda_seq, nlambda = nlambda, nfolds = 5)
  } else {
    cv_args <- list(nlambda = nlambda, nfolds = 10)
  }
  
  if (outcome_model == "t-learner") {
    # Separate models for treatment and control
    train_control <- train_data[train_data[[treatment_indicator]] == 0, ]
    train_treated <- train_data[train_data[[treatment_indicator]] == 1, ]
    
    X_train_control <- X_train[train_data[[treatment_indicator]] == 0, ]
    X_train_treated <- X_train[train_data[[treatment_indicator]] == 1, ]
    
    # Check if there are enough observations in each group
    if (nrow(train_control) > 0 && nrow(train_treated) > 0) {
      # Fit models for control and treated groups
      args_control <- c(list(x = X_train_control, 
                            y = Surv(train_control[[tstart]], 
                                   train_control[[tstop]], 
                                   train_control[[event_indicator]]),
                       family = "cox"), cv_args)
      
      args_treated <- c(list(x = X_train_treated, 
                           y = Surv(train_treated[[tstart]], 
                                  train_treated[[tstop]], 
                                  train_treated[[event_indicator]]),
                      family = "cox"), cv_args)
      
      model_control <- do.call(cv.glmnet, args_control)
      model_treated <- do.call(cv.glmnet, args_treated)
      
      # Make predictions
      y0_pred <- predict(model_control, newx = X_test, s = "lambda.min")
      y1_pred <- predict(model_treated, newx = X_test, s = "lambda.min")
      
      # Get coefficients
      beta_control <- coef(model_control, s = "lambda.min")
      beta_treated <- coef(model_treated, s = "lambda.min")
      
      # Calculate treatment effect
      beta_treatment <- beta_treated - beta_control
    } else {
      # Fall back to s-learner if one group has no observations
      warning("Insufficient data for T-learner, falling back to S-learner")
      outcome_model <- "s-learner"
    }
  }
  
  if (outcome_model == "s-learner") {
    # Single model with treatment as a predictor
    X_train_s <- cbind(X_train, train_data[[treatment_indicator]])
    colnames(X_train_s)[ncol(X_train_s)] <- treatment_indicator
    
    # Add interactions between treatment and covariates
    for (i in 1:ncol(X_train)) {
      X_train_s <- cbind(X_train_s, 
                         X_train[, i] * train_data[[treatment_indicator]])
      colnames(X_train_s)[ncol(X_train_s)] <- paste0(colnames(X_train)[i], 
                                                    "_by_", treatment_indicator)
    }
    
    # Fit model
    args_s <- c(list(x = X_train_s, 
                    y = surv_train,
                    family = "cox"), cv_args)
    
    model_s <- do.call(cv.glmnet, args_s)
    
    # Extract coefficients
    beta_s <- coef(model_s, s = "lambda.min")
    
    # Get indices of treatment-related coefficients
    tx_idx <- which(names(beta_s) == treatment_indicator)
    interaction_idx <- grep(paste0("_by_", treatment_indicator), names(beta_s))
    
    # Construct test matrices for predictions
    X_test_s0 <- cbind(X_test, 0)
    colnames(X_test_s0)[ncol(X_test_s0)] <- treatment_indicator
    
    X_test_s1 <- cbind(X_test, 1)
    colnames(X_test_s1)[ncol(X_test_s1)] <- treatment_indicator
    
    # Add interactions for prediction matrices
    for (i in 1:ncol(X_test)) {
      X_test_s0 <- cbind(X_test_s0, X_test[, i] * 0)
      X_test_s1 <- cbind(X_test_s1, X_test[, i] * 1)
      colnames(X_test_s0)[ncol(X_test_s0)] <- paste0(colnames(X_test)[i], 
                                                   "_by_", treatment_indicator)
      colnames(X_test_s1)[ncol(X_test_s1)] <- paste0(colnames(X_test)[i], 
                                                   "_by_", treatment_indicator)
    }
    
    # Make predictions
    y0_pred <- predict(model_s, newx = X_test_s0, s = "lambda.min")
    y1_pred <- predict(model_s, newx = X_test_s1, s = "lambda.min")
    
    # Extract treatment effect coefficients
    beta_control <- beta_s[-c(tx_idx, interaction_idx)]
    beta_treatment <- c(beta_s[tx_idx], beta_s[interaction_idx])
  }
  
  if (outcome_model == "m-learner") {
    # Marginal model ignoring treatment
    args_m <- c(list(x = X_train, 
                    y = surv_train,
                    family = "cox"), cv_args)
    
    model_m <- do.call(cv.glmnet, args_m)
    
    # Make predictions (same for both potential outcomes)
    y_pred <- predict(model_m, newx = X_test, s = "lambda.min")
    y0_pred <- y1_pred <- y_pred
    
    # Extract coefficients
    beta_control <- coef(model_m, s = "lambda.min")
    beta_treatment <- rep(0, length(beta_control))  # No treatment effect in m-learner
  }
  
  # Prepare return values
  result <- list(
    outcome_model = outcome_model,
    transform_type = transform_type,
    y0_pred = y0_pred,
    y1_pred = y1_pred,
    beta_control = beta_control,
    beta_treatment = beta_treatment
  )
  
  # Add model objects based on the outcome_model type
  if (outcome_model == "t-learner") {
    result$model_control <- model_control
    result$model_treated <- model_treated
  } else if (outcome_model == "s-learner") {
    result$model <- model_s
  } else if (outcome_model == "m-learner") {
    result$model <- model_m
  }
  
  return(result)
}