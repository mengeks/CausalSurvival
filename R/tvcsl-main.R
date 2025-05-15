#' @title Time-Varying Causal Survival Learning (TV-CSL): Main Interface
#' @description Main functions for fitting and using TV-CSL models.
#'
#' @importFrom survival coxph Surv
#' @importFrom stats as.formula terms coef predict model.matrix
#' @importFrom utils head tail
#' @importFrom stats na.omit

# Global variables
.SUPPORTED_TREATMENT_FORMS <- c("constant", "linear")
.SUPPORTED_BASELINE_FORMS <- c("linear", "nonlinear")
.SUPPORTED_PROPENSITY_MODELS <- c("cox-linear", "cox-nonlinear")
.SUPPORTED_OUTCOME_MODELS <- c("t-learner", "s-learner", "m-learner")
.SUPPORTED_FINAL_MODEL_METHODS <- c("coxph", "lasso-coxph")

#' Time-Varying Causal Survival Learning (TV-CSL)
#'
#' @description Estimates causal effects of time-varying treatments in survival analysis
#' using double machine learning methods.
#'
#' @param formula A formula specifying the outcome model. The left-hand side should be a
#'        Surv object, and the right-hand side should include covariates.
#' @param data A data frame containing the variables in the formula.
#' @param treatment_time Name of the column containing treatment times.
#' @param treatment_effect_form Form of the treatment effect (default: "constant").
#'        - "constant": Homogeneous treatment effect.
#'        - "linear": Treatment effect varies linearly with covariates.
#' @param baseline_form Form of the baseline hazard model (default: "linear").
#'        - "linear": Linear terms only.
#'        - "nonlinear": Includes splines, interactions, etc.
#' @param propensity_model Type of model for propensity score estimation (default: "cox-linear").
#'        - "cox-linear": Cox model with linear terms.
#'        - "cox-nonlinear": Cox model with nonlinear terms.
#' @param outcome_model Type of outcome model (default: "s-learner").
#'        - "t-learner": Separate models for treated and control.
#'        - "s-learner": Single model with treatment as predictor.
#'        - "m-learner": Marginal model ignoring treatment.
#' @param final_model_method Method for final model fitting (default: "coxph").
#'        - "coxph": Standard Cox model.
#'        - "lasso-coxph": Lasso-penalized Cox model.
#' @param cv_folds Number of folds for cross-fitting (default: 5).
#' @param id Name of the column containing subject IDs (default: "id").
#' @param fast_lasso Whether to use faster but less exhaustive lasso CV (default: TRUE).
#' @param return_cv_models Whether to return all cross-validation models (default: FALSE).
#' @param verbose Whether to print progress messages (default: FALSE).
#'
#' @return A tvcsl object containing the fitted model and results.
#'
#' @examples
#' \dontrun{
#' # Create time-varying dataset
#' data_tv <- create_time_varying_dataset(
#'   data = heart_data,
#'   event_time = "time_to_death",
#'   event_indicator = "death",
#'   treatment_time = "time_to_transplant",
#'   covariates = c("age", "gender", "smoker")
#' )
#'
#' # Fit TV-CSL model
#' model <- tvcsl(
#'   formula = Surv(tstart, tstop, event_indicator) ~ age + gender + smoker,
#'   data = data_tv,
#'   treatment_time = "time_to_tx",
#'   treatment_effect_form = "linear"
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Make predictions
#' predictions <- predict(model, newdata = test_data)
#' }
#'
#' @export
tvcsl <- function(formula,
                 data,
                 treatment_time,
                 treatment_effect_form = "constant",
                 baseline_form = "linear",
                 propensity_model = "cox-linear",
                 outcome_model = "s-learner",
                 final_model_method = "coxph",
                 cv_folds = 5,
                 id = "id",
                 fast_lasso = TRUE,
                 return_cv_models = FALSE,
                 verbose = FALSE) {
  
  # Check inputs
  if (!treatment_effect_form %in% .SUPPORTED_TREATMENT_FORMS) {
    stop("Invalid treatment_effect_form. Supported values: ",
         paste(.SUPPORTED_TREATMENT_FORMS, collapse = ", "))
  }
  
  if (!baseline_form %in% .SUPPORTED_BASELINE_FORMS) {
    stop("Invalid baseline_form. Supported values: ",
         paste(.SUPPORTED_BASELINE_FORMS, collapse = ", "))
  }
  
  if (!propensity_model %in% .SUPPORTED_PROPENSITY_MODELS) {
    stop("Invalid propensity_model. Supported values: ",
         paste(.SUPPORTED_PROPENSITY_MODELS, collapse = ", "))
  }
  
  if (!outcome_model %in% .SUPPORTED_OUTCOME_MODELS) {
    stop("Invalid outcome_model. Supported values: ",
         paste(.SUPPORTED_OUTCOME_MODELS, collapse = ", "))
  }
  
  if (!final_model_method %in% .SUPPORTED_FINAL_MODEL_METHODS) {
    stop("Invalid final_model_method. Supported values: ",
         paste(.SUPPORTED_FINAL_MODEL_METHODS, collapse = ", "))
  }
  
  if (!treatment_time %in% colnames(data)) {
    stop("Treatment time column '", treatment_time, "' not found in data")
  }
  
  if (!id %in% colnames(data)) {
    # Add ID column if it doesn't exist
    data[[id]] <- 1:nrow(data)
    warning("ID column not found. Creating sequential IDs.")
  }
  
  # Extract components from formula
  formula_terms <- terms(formula)
  if (!inherits(formula[[2]], "call") || formula[[2]][[1]] != as.name("Surv")) {
    stop("Left-hand side of formula must be a Surv object")
  }
  
  # Extract column names from Surv object
  surv_args <- as.list(formula[[2]])
  if (length(surv_args) == 4) {
    # Surv(tstart, tstop, event)
    tstart <- as.character(surv_args[[2]])
    tstop <- as.character(surv_args[[3]])
    event_indicator <- as.character(surv_args[[4]])
  } else if (length(surv_args) == 3) {
    # Surv(time, event)
    tstart <- NULL
    tstop <- as.character(surv_args[[2]])
    event_indicator <- as.character(surv_args[[3]])
  } else {
    stop("Invalid Surv specification in formula")
  }
  
  # Check if data is already in time-varying format
  if (is.null(tstart) || !tstart %in% colnames(data)) {
    message("Converting data to time-varying format...")
    if (is.null(tstart)) {
      tstart <- "tstart"
    }
    
    # Create time-varying dataset
    data <- create_time_varying_dataset(
      data = data,
      event_time = tstop,
      event_indicator = event_indicator,
      treatment_time = treatment_time,
      id = id
    )
    
    # Update column names for standardized processing
    tstop <- "tstop"
    event_indicator <- "event_indicator"
    treatment_indicator <- "treatment_indicator"
  } else {
    # Data is already in time-varying format, but need to ensure treatment_indicator column
    if (!"treatment_indicator" %in% colnames(data)) {
      # Try to infer treatment indicator
      if (paste0(treatment_time, "_indicator") %in% colnames(data)) {
        treatment_indicator <- paste0(treatment_time, "_indicator")
      } else if ("trt" %in% colnames(data)) {
        # If "trt" column exists, use it and create a treatment_indicator column
        data$treatment_indicator <- data$trt
        treatment_indicator <- "treatment_indicator"
      } else {
        stop("Cannot find treatment indicator column. Please add a treatment_indicator column.")
      }
    } else {
      treatment_indicator <- "treatment_indicator"
    }
  }
  
  # Extract covariates from formula
  covariates <- attr(formula_terms, "term.labels")
  covariates <- covariates[!covariates %in% c(treatment_indicator, treatment_time)]
  
  # Check if covariates exist in data
  missing_covs <- setdiff(covariates, colnames(data))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in data: ", paste(missing_covs, collapse = ", "))
  }
  
  # Set lasso options based on fast_lasso parameter
  if (fast_lasso) {
    lambda_seq <- exp(seq(log(0.001), log(1), length.out = 20))
    nlambda <- 20
  } else {
    lambda_seq <- NULL
    nlambda <- 100
  }
  
  # Create folds for cross-fitting
  folds <- create_cv_folds(nrow(data), k = cv_folds, id = data[[id]])
  
  # Initialize lists to store results
  nuisance_models <- vector("list", cv_folds)
  propensity_models <- vector("list", cv_folds)
  final_models <- vector("list", cv_folds)
  treatment_effects <- vector("list", cv_folds)
  
  # Perform cross-fitting
  for (k in 1:cv_folds) {
    if (verbose) {
      message("Processing fold ", k, " of ", cv_folds)
    }
    
    # Split data into training and validation sets
    train_idx <- unlist(folds[-k])
    valid_idx <- folds[[k]]
    
    train_data <- data[train_idx, ]
    valid_data <- data[valid_idx, ]
    
    # Step 1: Estimate propensity scores
    if (verbose) {
      message("  Estimating propensity scores...")
    }
    
    propensity_models[[k]] <- estimate_propensity_scores(
      data = train_data,
      covariates = covariates,
      treatment_time = treatment_time,
      event_time = tstop,
      event_indicator = event_indicator,
      model_type = propensity_model,
      use_censored = TRUE
    )
    
    # Calculate propensity scores for validation data
    valid_ps <- calculate_propensity_scores(
      data = valid_data,
      ps_model = propensity_models[[k]],
      time_points = valid_data[[tstop]]
    )
    
    # Extract diagonal of propensity score matrix (score at event time)
    valid_data$propensity_score <- diag(valid_ps)
    
    # Step 2: Estimate nuisance parameters (outcome models)
    if (verbose) {
      message("  Estimating outcome models...")
    }
    
    nuisance_models[[k]] <- estimate_nuisance_parameters(
      train_data = train_data,
      test_data = valid_data,
      outcome_model = outcome_model,
      transform_type = baseline_form,
      covariates = covariates,
      treatment_indicator = treatment_indicator,
      tstart = tstart,
      tstop = tstop,
      event_indicator = event_indicator,
      lambda_seq = lambda_seq,
      nlambda = nlambda,
      fast_cv = fast_lasso
    )
    
    # Step 3: Fit final model
    if (verbose) {
      message("  Fitting final model...")
    }
    
    final_models[[k]] <- fit_final_model(
      data = valid_data,
      propensity_scores = valid_data$propensity_score,
      nuisance_estimates = nuisance_models[[k]],
      treatment_effect_form = treatment_effect_form,
      covariates = covariates,
      treatment_indicator = treatment_indicator,
      tstart = tstart,
      tstop = tstop,
      event_indicator = event_indicator,
      method = final_model_method,
      lambda_seq = lambda_seq
    )
    
    # Step 4: Calculate treatment effects
    if (verbose) {
      message("  Calculating treatment effects...")
    }
    
    treatment_effects[[k]] <- list(
      coefficients = final_models[[k]]$coefficients,
      model = final_models[[k]]
    )
  }
  
  # Aggregate results across folds
  if (verbose) {
    message("Aggregating results...")
  }
  
  # Average coefficients across folds
  coef_matrices <- lapply(treatment_effects, function(x) {
    as.matrix(x$coefficients)
  })
  avg_coefficients <- Reduce(`+`, coef_matrices) / length(coef_matrices)
  
  # Create final model object
  result <- list(
    coefficients = avg_coefficients,
    formula = formula,
    data = data,
    treatment_time = treatment_time,
    treatment_indicator = treatment_indicator,
    treatment_effect_form = treatment_effect_form,
    baseline_form = baseline_form,
    propensity_model = propensity_model,
    outcome_model = outcome_model,
    final_model_method = final_model_method,
    covariates = covariates,
    cv_folds = cv_folds,
    tstart = tstart,
    tstop = tstop,
    event_indicator = event_indicator
  )
  
  # Add cross-validation models if requested
  if (return_cv_models) {
    result$propensity_models <- propensity_models
    result$nuisance_models <- nuisance_models
    result$final_models <- final_models
  }
  
  class(result) <- "tvcsl"
  return(result)
}

#' Fit a TV-CSL model with default settings
#'
#' @description A simplified interface for fitting a TV-CSL model with sensible defaults
#'
#' @param data A data frame containing the survival data
#' @param event_time Column name for time to event
#' @param event_indicator Column name for event indicator
#' @param treatment_time Column name for time to treatment
#' @param covariates Character vector of covariates to use
#' @param id Column name for subject identifier (default: "id")
#' @param treatment_effect_form Form of treatment effect (default: "constant")
#'
#' @return A tvcsl object
#'
#' @examples
#' \dontrun{
#' # Fit a TV-CSL model with minimal specification
#' model <- quick_tvcsl(
#'   data = heart_data,
#'   event_time = "time_to_death",
#'   event_indicator = "death",
#'   treatment_time = "time_to_transplant",
#'   covariates = c("age", "gender", "smoker")
#' )
#' }
#'
#' @export
quick_tvcsl <- function(data,
                       event_time,
                       event_indicator,
                       treatment_time,
                       covariates,
                       id = "id",
                       treatment_effect_form = "constant") {
  
  # Create formula
  formula_str <- paste0("Surv(", event_time, ", ", event_indicator, ") ~ ",
                       paste(covariates, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit model
  model <- tvcsl(
    formula = formula,
    data = data,
    treatment_time = treatment_time,
    treatment_effect_form = treatment_effect_form,
    baseline_form = "linear",
    propensity_model = "cox-linear",
    outcome_model = "s-learner",
    final_model_method = "coxph",
    cv_folds = 5,
    id = id,
    fast_lasso = TRUE
  )
  
  return(model)
}

#' Summary method for tvcsl objects
#'
#' @param object A tvcsl object
#' @param ... Additional arguments (not used)
#'
#' @return A summary object
#'
#' @export
summary.tvcsl <- function(object, ...) {
  cat("Time-Varying Causal Survival Learning (TV-CSL)\n")
  cat("--------------------------------------------\n")
  
  cat("\nModel Specification:\n")
  cat("  Treatment effect form: ", object$treatment_effect_form, "\n")
  cat("  Baseline hazard form: ", object$baseline_form, "\n")
  cat("  Propensity model: ", object$propensity_model, "\n")
  cat("  Outcome model: ", object$outcome_model, "\n")
  cat("  Final model method: ", object$final_model_method, "\n")
  cat("  Cross-validation folds: ", object$cv_folds, "\n")
  
  cat("\nCovariates:\n")
  if (length(object$covariates) > 0) {
    cat("  ", paste(object$covariates, collapse = ", "), "\n")
  } else {
    cat("  None\n")
  }
  
  cat("\nTreatment Effect Coefficients:\n")
  if (object$treatment_effect_form == "constant") {
    cat("  Constant effect: ", round(object$coefficients[1], 4), "\n")
  } else if (object$treatment_effect_form == "linear") {
    cat("  Intercept: ", round(object$coefficients[1], 4), "\n")
    for (i in 2:length(object$coefficients)) {
      cat("  ", object$covariates[i-1], ": ", round(object$coefficients[i], 4), "\n")
    }
  }
  
  cat("\n")
  invisible(object)
}

#' Print method for tvcsl objects
#'
#' @param x A tvcsl object
#' @param ... Additional arguments (not used)
#'
#' @return The tvcsl object (invisibly)
#'
#' @export
print.tvcsl <- function(x, ...) {
  cat("Time-Varying Causal Survival Learning (TV-CSL)\n")
  cat("\nTreatment Effect Coefficients:\n")
  if (x$treatment_effect_form == "constant") {
    cat("  Constant effect: ", round(x$coefficients[1], 4), "\n")
  } else if (x$treatment_effect_form == "linear") {
    cat("  Intercept: ", round(x$coefficients[1], 4), "\n")
    if (length(x$coefficients) > 1) {
      for (i in 2:length(x$coefficients)) {
        cat("  ", x$covariates[i-1], ": ", round(x$coefficients[i], 4), "\n")
      }
    }
  }
  
  cat("\nUse summary() for more details\n")
  invisible(x)
}

#' Predict method for tvcsl objects
#'
#' @param object A tvcsl object
#' @param newdata A data frame containing data for prediction
#' @param ... Additional arguments (not used)
#'
#' @return A vector of predicted treatment effects
#'
#' @export
predict.tvcsl <- function(object, newdata, ...) {
  # Check if required columns exist
  missing_cols <- setdiff(object$covariates, colnames(newdata))
  if (length(missing_cols) > 0) {
    stop("Missing columns in newdata: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create design matrix for prediction
  if (object$treatment_effect_form == "constant") {
    # Only intercept
    X_pred <- matrix(1, nrow = nrow(newdata), ncol = 1)
  } else if (object$treatment_effect_form == "linear") {
    # Intercept plus covariates
    X_pred <- cbind(1, as.matrix(newdata[, object$covariates, drop = FALSE]))
  }
  
  # Calculate predictions
  predictions <- as.vector(X_pred %*% object$coefficients)
  
  return(predictions)
}