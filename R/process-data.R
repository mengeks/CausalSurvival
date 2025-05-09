#' Create a dataset for time-varying Cox model analysis
#'
#' This function transforms a standard survival dataset into a format suitable for
#' time-varying covariate analysis in survival models, similar to the `tmerge` function
#' in the survival package.
#'
#' @param data A data frame containing the original survival data
#' @param event_time Column name for time to event
#' @param event Column name for event indicator (1=event, 0=censored)
#' @param tx_time Column name for time to treatment/intervention
#' @param covariates Character vector of column names for covariates to retain
#' @param id_col Column name for subject identifier (default: "id")
#'
#' @return A data frame in (start, stop] format suitable for time-varying Cox models
#'
#' @examples
#' # Example usage:
#' # data_tv <- create_time_varying_dataset(
#' #   data = my_data,
#' #   event_time = "time_to_event",
#' #   event = "death",
#' #   tx_time = "time_to_tx",
#' #   covariates = c("age", "sex", "surgery")
#' # )
#'
#' @export
create_time_varying_dataset <- function(data, 
                                        event_time,
                                        event,
                                        tx_time,
                                        covariates = NULL,
                                        id_col = "id") {
  
  # Ensure column names are properly quoted
  event_time_sym <- rlang::ensym(event_time)
  event_sym <- rlang::ensym(event)
  tx_time_sym <- rlang::ensym(tx_time)
  id_sym <- rlang::ensym(id_col)
  
  # Handle cases where covariates is NULL
  if (is.null(covariates)) {
    covariates <- setdiff(colnames(data), 
                          c(as.character(event_time_sym), 
                            as.character(event_sym), 
                            as.character(tx_time_sym), 
                            as.character(id_sym)))
  }
  
  # Create empty result dataframe
  result <- data.frame()
  
  # Process each subject
  for (i in 1:nrow(data)) {
    # Extract values for current subject
    row_data <- data[i, , drop = FALSE]
    t_event <- row_data[[as.character(event_time_sym)]]
    event_status <- row_data[[as.character(event_sym)]]
    t_tx <- row_data[[as.character(tx_time_sym)]]
    
    # Check for NA or missing tx time - treat as infinity
    if (is.na(t_tx)) {
      t_tx <- Inf
    }
    
    # Case 1: Subject did not receive treatment before event/censoring
    if (t_event <= t_tx) {
      new_row <- row_data
      new_row$tstart <- 0
      new_row$tstop <- t_event
      new_row$status <- event_status
      new_row$trt <- 0
    } 
    # Case 2: Subject received treatment before event/censoring
    else {
      # First interval: from 0 to treatment time
      new_row1 <- row_data
      new_row1$tstart <- 0
      new_row1$tstop <- t_tx
      new_row1$status <- 0  # No event in this interval
      new_row1$trt <- 0     # Not treated yet
      
      # Second interval: from treatment time to event/censoring
      new_row2 <- row_data
      new_row2$tstart <- t_tx
      new_row2$tstop <- t_event
      new_row2$status <- event_status
      new_row2$trt <- 1     # Treated in this interval
      
      # Combine rows
      new_row <- rbind(new_row1, new_row2)
    }
    
    # Add to result
    result <- rbind(result, new_row)
  }
  
  # Select only needed columns
  result <- result[, c(id_col, "tstart", "tstop", "status", "trt", covariates)]
  
  # Ensure time intervals are valid (no zero-length intervals)
  result <- result[result$tstop > result$tstart, ]
  
  return(result)
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
generate_regressor_part <- 
  function(model_spec = "correctly-specified", 
           HTE_type = "constant", 
           eta_type = "linear-interaction") {
    
  # Handle W based on HTE_type
  if (HTE_type == "constant") {
    W_part <- "W"
  } else if (HTE_type == "linear") {
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
                               HTE_type = "constant",
                               eta_type = "linear-interaction") {
  regressor_part <- 
    generate_regressor_part(model_spec = model_spec,
                            HTE_type = HTE_type,
                            eta_type = eta_type)
  
  if (run_time_varying) {
    formula <- as.formula(paste("Surv(tstart, tstop, Delta) ~", regressor_part))
  } else {
    formula <- as.formula(paste("Surv(U, Delta) ~", regressor_part))
  }
  
  return(formula)
}


