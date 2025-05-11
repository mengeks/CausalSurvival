#' @title Time-Varying Causal Survival Learning (TV-CSL): Feature Transformation
#' @description Functions for transforming features for flexible modeling in survival analysis.
#'
#' @importFrom splines ns
#' @importFrom dplyr select %>%

#' Transform covariates for model fitting
#'
#' @description Creates transformed features from the original covariates
#' to allow for flexible modeling. Transformations include linear terms,
#' natural splines, squared terms, and interaction terms.
#'
#' @param data A data frame containing the covariates
#' @param transform_type Type of transformation to apply (default: "linear")
#'   - "linear": Only linear terms
#'   - "nonlinear": Natural splines, squared terms, and interactions
#' @param covariates Names of covariate columns to transform (default: NULL, uses all columns)
#' @param degrees_of_freedom Degrees of freedom for natural splines (default: 3)
#'
#' @return A matrix of transformed features
#'
#' @keywords internal
transform_covariates <- function(data, 
                                transform_type = "linear",
                                covariates = NULL,
                                degrees_of_freedom = 3) {
  
  if (is.null(covariates)) {
    # Use all numeric columns as covariates
    numeric_cols <- sapply(data, is.numeric)
    covariates <- names(data)[numeric_cols]
  }
  
  # Extract covariates
  X_data <- data[, covariates, drop = FALSE]
  
  if (transform_type == "linear") {
    return(as.matrix(X_data))
  } else if (transform_type == "nonlinear") {
    transformed_X <- matrix(nrow = nrow(X_data), ncol = 0)
    
    # Add linear terms
    transformed_X <- cbind(transformed_X, as.matrix(X_data))
    
    # Identify continuous variables (more than 5 unique values)
    continuous_vars <- names(X_data)[sapply(X_data, function(x) length(unique(x)) > 5)]
    
    # Apply natural splines to continuous variables
    for (var_name in continuous_vars) {
      spline_terms <- ns(X_data[[var_name]], df = degrees_of_freedom)
      colnames(spline_terms) <- paste0(var_name, "_spline", 1:degrees_of_freedom)
      transformed_X <- cbind(transformed_X, spline_terms)
    }
    
    # Add square terms for continuous variables
    for (var_name in continuous_vars) {
      square_term <- X_data[[var_name]]^2
      transformed_X <- cbind(transformed_X, square_term)
      colnames(transformed_X)[ncol(transformed_X)] <- paste0(var_name, "_squared")
    }
    
    # Add interaction terms between pairs of variables
    var_names <- names(X_data)
    for (i in 1:(length(var_names) - 1)) {
      for (j in (i + 1):length(var_names)) {
        interaction_term <- X_data[[var_names[i]]] * X_data[[var_names[j]]]
        transformed_X <- cbind(transformed_X, interaction_term)
        colnames(transformed_X)[ncol(transformed_X)] <- paste0(var_names[i], "_x_", var_names[j])
      }
    }
    
    return(transformed_X)
  } else {
    stop("Invalid transform_type. Choose 'linear' or 'nonlinear'.")
  }
}