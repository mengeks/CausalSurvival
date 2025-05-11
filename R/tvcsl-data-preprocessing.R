#' @title Time-Varying Causal Survival Learning (TV-CSL): Data Preprocessing
#' @description Functions for preparing data for time-varying causal survival analysis.
#'
#' @importFrom dplyr mutate filter select %>%

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
  
  # Input validation
  if (!event_time %in% colnames(data)) {
    stop(paste("Event time column", event_time, "not found in data"))
  }
  if (!event %in% colnames(data)) {
    stop(paste("Event indicator column", event, "not found in data"))
  }
  if (!tx_time %in% colnames(data)) {
    stop(paste("Treatment time column", tx_time, "not found in data"))
  }
  if (!id_col %in% colnames(data)) {
    stop(paste("ID column", id_col, "not found in data"))
  }
  
  # Handle cases where covariates is NULL
  if (is.null(covariates)) {
    covariates <- setdiff(colnames(data), 
                          c(event_time, event, tx_time, id_col))
  } else {
    # Validate that all specified covariates exist
    missing_covs <- setdiff(covariates, colnames(data))
    if (length(missing_covs) > 0) {
      stop(paste("Covariates not found in data:", 
                 paste(missing_covs, collapse = ", ")))
    }
  }
  
  # Create empty result dataframe
  result <- data.frame()
  
  # Process each subject
  for (i in 1:nrow(data)) {
    # Extract values for current subject
    row_data <- data[i, , drop = FALSE]
    t_event <- row_data[[event_time]]
    event_status <- row_data[[event]]
    t_tx <- row_data[[tx_time]]
    
    # Skip rows with NA event times
    if (is.na(t_event)) {
      warning(paste("Row", i, "skipped: missing event time"))
      next
    }
    
    # Check for NA event status
    if (is.na(event_status)) {
      warning(paste("Row", i, "skipped: missing event status"))
      next
    }
    
    # Check for NA or missing tx time - treat as infinity
    if (is.na(t_tx)) {
      t_tx <- Inf
    }
    
    # Case 1: Subject did not receive treatment before event/censoring
    # This includes the case where treatment happens at the same time as event
    if (t_event <= t_tx) {
      new_row <- row_data
      new_row$tstart <- 0
      new_row$tstop <- t_event
      new_row$status <- event_status
      new_row$trt <- 0  # Not treated during observation period
      
      # Add to result
      result <- rbind(result, new_row)
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
      
      # Add both rows to result
      result <- rbind(result, new_row1, new_row2)
    }
  }
  
  # Handle empty result (all rows had missing values)
  if (nrow(result) == 0) {
    warning("No valid observations found after handling missing values.")
    return(data.frame())
  }
  
  # Ensure time intervals are valid (no zero-length intervals)
  result <- result[result$tstop > result$tstart, ]
  
  # Select only needed columns
  cols_to_keep <- unique(c(id_col, "tstart", "tstop", "status", "trt", covariates))
  cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(result)]
  result <- result[, cols_to_keep]
  
  return(result)
}
#' Create pseudo-dataset from standard survival data
#'
#' @description Transforms a standard survival dataset into a format suitable for
#' the TV-CSL algorithm. This is an internal function called by tvcsl().
#'
#' @param survival_data A data frame with survival data
#' @param event_time Column name for time to event (default: "U")
#' @param event_indicator Column name for event indicator (default: "Delta")
#' @param treatment_time Column name for time to treatment (default: "A")
#' @param treatment_indicator Column name for treatment indicator (default: "W")
#' @param id Column name for subject identifier (default: "id")
#'
#' @return A data frame in (start, stop] format suitable for TV-CSL
#'
#' @keywords internal
create_pseudo_dataset <- function(survival_data,
                                  event_time = "U",
                                  event_indicator = "Delta",
                                  treatment_time = "A",
                                  treatment_indicator = "W",
                                  id = "id") {
  
  # If the input is already in time-varying format, return it
  if (all(c("tstart", "tstop") %in% colnames(survival_data))) {
    # Ensure treatment_indicator column exists
    if (!treatment_indicator %in% colnames(survival_data)) {
      survival_data[[treatment_indicator]] <- 0
      warning("Treatment indicator column not found. Adding default column with all zeros.")
    }
    return(survival_data)
  }
  
  # Process each subject to create time-varying dataset
  result <- data.frame()
  
  for (i in 1:nrow(survival_data)) {
    # Extract values for current subject
    row_data <- survival_data[i, , drop = FALSE]
    t_event <- row_data[[event_time]]
    event_status <- row_data[[event_indicator]]
    t_tx <- row_data[[treatment_time]]
    
    # Skip rows with NA event times
    if (is.na(t_event) || is.na(event_status)) {
      next
    }
    
    # Case 1: Subject did not receive treatment before event/censoring
    if (t_event <= t_tx || is.na(t_tx) || is.infinite(t_tx)) {
      new_row <- row_data
      new_row$tstart <- 0
      new_row$tstop <- t_event
      new_row[[event_indicator]] <- event_status
      new_row[[treatment_indicator]] <- 0  # Not treated during observation period
      
      # Add to result
      result <- rbind(result, new_row)
    } 
    # Case 2: Subject received treatment before event/censoring
    else {
      # First interval: from 0 to treatment time
      new_row1 <- row_data
      new_row1$tstart <- 0
      new_row1$tstop <- t_tx
      new_row1[[event_indicator]] <- 0  # No event in this interval
      new_row1[[treatment_indicator]] <- 0     # Not treated yet
      
      # Second interval: from treatment time to event/censoring
      new_row2 <- row_data
      new_row2$tstart <- t_tx
      new_row2$tstop <- t_event
      new_row2[[event_indicator]] <- event_status
      new_row2[[treatment_indicator]] <- 1     # Treated in this interval
      
      # Add both rows to result
      result <- rbind(result, new_row1, new_row2)
    }
  }
  
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


