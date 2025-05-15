# TV-CSL: Data Simulation Functions
# This file contains the data simulation functions used to generate test data for the TV-CSL package.

#' Generate simple simulated data for TV-CSL examples
#'
#' @description Creates a simulated dataset with time-to-event outcomes and time-varying treatments
#' suitable for testing the TV-CSL package.
#'
#' @param n Number of observations to generate (default: 500)
#' @param treatment_effect Type of treatment effect ("constant", "linear", or "non-linear")
#' @param baseline_form Type of baseline hazard function ("linear" or "non-linear")
#' @param seed Random seed for reproducibility
#'
#' @return A data frame containing simulated survival data
#'
#' @examples
#' data <- generate_simple_data(n = 200, treatment_effect = "linear")
#' head(data)
#'
#' @export
generate_simple_data <- function(n = 500, 
                                treatment_effect = "linear", 
                                baseline_form = "non-linear",
                                seed = 123) {
  
  set.seed(seed)
  
  # Generate covariates
  data <- data.frame(
    id = 1:n,
    age = rnorm(n, mean = 50, sd = 10),
    comorbidity = rpois(n, lambda = 2),
    biomarker = rnorm(n, mean = 1, sd = 0.3)
  )
  
  # Scale covariates for numeric stability
  data$age_scaled <- scale(data$age)
  data$biomarker_scaled <- scale(data$biomarker)
  
  # Baseline hazard based on form
  if (baseline_form == "linear") {
    eta_0 <- with(data, 0.5 + 0.2 * age_scaled + 0.1 * comorbidity - 0.3 * biomarker_scaled)
  } else {
    # Non-linear baseline hazard
    eta_0 <- with(data, 0.5 + 0.3 * age_scaled^2 + 
                  0.2 * log(comorbidity + 1) - 0.3 * sin(biomarker_scaled * pi))
  }
  
  # Treatment effect based on type
  if (treatment_effect == "constant") {
    tau <- rep(-0.5, n)  # Constant treatment effect (negative = beneficial)
  } else if (treatment_effect == "linear") {
    tau <- with(data, -1 + 0.4 * age_scaled + 0.1 * comorbidity)  # Linear in covariates
  } else {
    # Non-linear treatment effect
    tau <- with(data, -0.8 + 0.5 * age_scaled^2 - 0.3 * (biomarker_scaled > 0))
  }
  
  # Generate treatment times
  # Higher comorbidity -> earlier treatment
  # Higher age -> later treatment (older patients wait longer)
  tx_hazard <- with(data, exp(0.5 - 0.1 * age_scaled + 0.3 * comorbidity))
  data$time_to_tx <- rexp(n, rate = tx_hazard)
  
  # Make some subjects untreated
  untreated <- sample(1:n, n/5)  # 20% untreated
  data$time_to_tx[untreated] <- Inf
  
  # Generate survival times under control
  control_hazard <- exp(eta_0)
  control_time <- rexp(n, rate = control_hazard)
  
  # Generate survival times under treatment (using treatment effect)
  treated_hazard <- exp(eta_0 + tau)
  treated_time <- rexp(n, rate = treated_hazard)
  
  # Combine to get observed times
  data$time_to_event <- ifelse(data$time_to_tx < Inf, 
                              pmin(data$time_to_tx, control_time) + 
                                ifelse(data$time_to_tx < control_time, 
                                      treated_time, 0),
                              control_time)
  
  # Add some censoring
  cens_time <- rexp(n, rate = 0.05)
  data$event <- as.integer(data$time_to_event <= cens_time)
  data$time_to_event <- pmin(data$time_to_event, cens_time)
  
  # Add true treatment effect for evaluation
  data$true_effect <- tau
  
  return(data)
}

#' Generate more complex simulated data for TV-CSL evaluations
#'
#' @description Creates a simulated dataset with time-to-event outcomes and time-varying treatments
#' with more complex data generation process, similar to simulation studies in the paper.
#'
#' @param n Number of observations to generate (default: 1000)
#' @param p Number of covariates (default: 10)
#' @param eta_type Baseline hazard type ("linear" or "non-linear")
#' @param HTE_type Treatment effect type ("zero", "constant", "linear", "non-linear")
#' @param lambda_C Censoring rate (default: 0.1)
#' @param seed Random seed for reproducibility
#'
#' @return A data frame containing simulated survival data
#'
#' @examples
#' data <- generate_complex_data(n = 500, HTE_type = "linear")
#' head(data)
#'
#' @export
generate_complex_data <- function(n = 1000, 
                                 p = 10,
                                 eta_type = "non-linear",
                                 HTE_type = "linear",
                                 lambda_C = 0.1,
                                 seed = 123) {
  
  set.seed(seed)
  
  # Generate covariates
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("X.", 1:p)
  
  # Create data frame
  data <- as.data.frame(X)
  data$id <- 1:n
  
  # Generate baseline hazard
  if (eta_type == "linear") {
    # Linear baseline hazard
    beta_eta <- c(0.5, 0.3, 0.1, -0.2, 0.15, rep(0, p-5))
    eta_0 <- 0.5 + as.matrix(data[, paste0("X.", 1:p)]) %*% beta_eta
  } else {
    # Non-linear baseline hazard
    eta_0 <- with(data, 0.5 + 
                 sqrt(abs(X.1 * X.2)) + 
                 sqrt(abs(X.10)) + 
                 cos(X.5) + 
                 cos(X.5) * cos(X.6))
  }
  
  # Generate treatment effect
  if (HTE_type == "zero") {
    tau <- rep(0, n)  # No treatment effect
  } else if (HTE_type == "constant") {
    tau <- rep(-0.5, n)  # Constant treatment effect
  } else if (HTE_type == "linear") {
    # Linear treatment effect depending on X.1 and X.10
    tau <- with(data, -0.8 + 0.4 * X.1 - 0.3 * X.10)
  } else {
    # Non-linear treatment effect
    tau <- with(data, -0.8 +
               0.5 * X.1^2 +
               0.5 * (X.10 > 0) - 
               0.3 * (X.1 > 0 & X.10 > 0))
  }
  
  # Generate treatment times
  # Treatment model uses X.1, X.2, and X.3
  alpha <- c(0.5, -0.2, 0.3, 0.15)
  tx_linear_pred <- 0.5 + X[, 1] * alpha[1] + X[, 2] * alpha[2] + X[, 3] * alpha[3]
  tx_hazard <- exp(tx_linear_pred)
  data$A <- rexp(n, rate = tx_hazard)
  
  # Make some subjects untreated
  untreated <- sample(1:n, n/5)  # 20% untreated
  data$A[untreated] <- Inf
  
  # Generate survival times under control
  lambda_0 <- 0.1  # Baseline hazard rate
  control_hazard <- lambda_0 * exp(eta_0)
  control_time <- rexp(n, rate = control_hazard)
  
  # Generate survival times under treatment (using treatment effect)
  treated_hazard <- lambda_0 * exp(eta_0 + tau)
  treated_time <- rexp(n, rate = treated_hazard)
  
  # Combine to get observed times
  data$T <- ifelse(data$A < Inf, 
                  pmin(data$A, control_time) + 
                    ifelse(data$A < control_time, 
                          treated_time, 0),
                  control_time)
  
  # Add censoring
  cens_time <- rexp(n, rate = lambda_C)
  data$Delta <- as.integer(data$T <= cens_time)
  data$U <- pmin(data$T, cens_time)
  
  # Add true treatment effect for evaluation
  data$HTE <- tau
  
  # Rename columns to be more descriptive
  names(data)[names(data) == "A"] <- "time_to_tx"
  names(data)[names(data) == "T"] <- "time_to_event_uncensored"
  names(data)[names(data) == "U"] <- "time_to_event"
  names(data)[names(data) == "Delta"] <- "event"
  
  return(data)
}
