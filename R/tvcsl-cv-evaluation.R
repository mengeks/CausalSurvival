#' @title Time-Varying Causal Survival Learning (TV-CSL): Cross-validation and Evaluation
#' @description Functions for cross-validation and evaluating model performance.
#'
#' @importFrom stats cor

#' Split data into folds for cross-validation
#'
#' @description Creates a list of indices for K-fold cross-validation
#'
#' @param n Number of observations
#' @param k Number of folds (default: 5)
#' @param id Optional vector of group IDs to ensure observations with same ID are in same fold
#'
#' @return A list of length k, where each element contains indices for one fold
#'
#' @keywords internal
create_cv_folds <- function(n, k = 5, id = NULL) {
  if (!is.null(id)) {
    # Group-based splitting to keep IDs together
    unique_ids <- unique(id)
    n_ids <- length(unique_ids)
    id_folds <- cut(sample(1:n_ids), breaks = k, labels = FALSE)
    
    # Assign observations to folds based on ID
    folds <- vector("list", k)
    for (i in 1:k) {
      fold_ids <- unique_ids[id_folds == i]
      folds[[i]] <- which(id %in% fold_ids)
    }
  } else {
    # Simple random splitting
    fold_indices <- sample(rep(1:k, length.out = n))
    folds <- vector("list", k)
    for (i in 1:k) {
      folds[[i]] <- which(fold_indices == i)
    }
  }
  
  return(folds)
}

#' Evaluate a fitted TV-CSL model
#'
#' @description Calculates performance metrics for a fitted TV-CSL model
#'
#' @param true_effects Vector of true treatment effects
#' @param predicted_effects Vector of predicted treatment effects
#' @param metrics Character vector of metrics to calculate (default: "mse")
#'
#' @return A named list of calculated metrics
#'
#' @keywords internal
evaluate_model <- function(true_effects,
                          predicted_effects,
                          metrics = "mse") {
  
  results <- list()
  
  if ("mse" %in% metrics || "all" %in% metrics) {
    results$mse <- mean((true_effects - predicted_effects)^2)
  }
  
  if ("mae" %in% metrics || "all" %in% metrics) {
    results$mae <- mean(abs(true_effects - predicted_effects))
  }
  
  if ("correlation" %in% metrics || "all" %in% metrics) {
    results$correlation <- cor(true_effects, predicted_effects)
  }
  
  if ("r2" %in% metrics || "all" %in% metrics) {
    ss_total <- sum((true_effects - mean(true_effects))^2)
    ss_residual <- sum((true_effects - predicted_effects)^2)
    results$r2 <- 1 - ss_residual / ss_total
  }
  
  return(results)
}

#' Evaluate TV-CSL model using test data with known treatment effects
#'
#' @description Calculates performance metrics for a fitted TV-CSL model
#' using test data with known true treatment effects
#'
#' @param object A tvcsl object
#' @param test_data Data frame containing test data
#' @param true_effects Column name for true treatment effects (default: "true_effect")
#' @param metrics Character vector of metrics to calculate (default: c("mse", "mae"))
#'
#' @return A list of performance metrics
#'
#' @export
evaluate_tvcsl <- function(object,
                          test_data,
                          true_effects = "true_effect",
                          metrics = c("mse", "mae")) {
  
  # Check if true effects column exists
  if (!true_effects %in% colnames(test_data)) {
    stop("True effects column '", true_effects, "' not found in test_data")
  }
  
  # Get predictions
  predicted_effects <- predict(object, test_data)
  
  # Calculate metrics
  results <- list()
  
  if ("mse" %in% metrics) {
    results$mse <- mean((test_data[[true_effects]] - predicted_effects)^2)
  }
  
  if ("mae" %in% metrics) {
    results$mae <- mean(abs(test_data[[true_effects]] - predicted_effects))
  }
  
  if ("correlation" %in% metrics) {
    results$correlation <- cor(test_data[[true_effects]], predicted_effects)
  }
  
  if ("r2" %in% metrics) {
    ss_total <- sum((test_data[[true_effects]] - mean(test_data[[true_effects]]))^2)
    ss_residual <- sum((test_data[[true_effects]] - predicted_effects)^2)
    results$r2 <- 1 - ss_residual / ss_total
  }
  
  return(results)
}