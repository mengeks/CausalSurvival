library(survival)
library(glmnet)
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


#' #' Create the Cox model formula
#' #'
#' #' @param run_time_varying Logical value. If TRUE, the time-varying Cox model is used.
#' #' @return A formula object for the Cox model.
#' #' @export
#' create_lasso_formula <- function(model_spec, 
#'                                  lasso_type, 
#'                                run_time_varying = T,
#'                                CATE_type = "constant",
#'                                eta_type = "linear-interaction") {
#'   regressor_part <- 
#'     generate_regressor_part(model_spec = model_spec,
#'                             lasso_type = lasso_type,
#'                             CATE_type = CATE_type,
#'                             eta_type = eta_type)
#'   
#'   if (run_time_varying) {
#'     formula <- as.formula(paste("Surv(tstart, tstop, Delta) ~", regressor_part))
#'   } else {
#'     formula <- as.formula(paste("Surv(U, Delta) ~", regressor_part))
#'   }
#'   
#'   return(formula)
#' }

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



#' #' Estimate Lasso model
#' #'
#' #' @param single_data The input dataset.
#' #' @param methods_cox A list containing model specification and run_time_varying flag.
#' #' @param light_censoring Optional parameter for light censoring.
#' #' @return The estimated coefficient for W in the Cox model.
#' #' @export
#' lasso_model_estimation <- function(
#'     single_data, 
#'     i,
#'     methods_lasso,
#'     CATE_type = "constant",
#'     eta_type = "linear-interaction"
#' ) {
#'   regressor_spec <- methods_lasso$regressor_spec
#'   lasso_type <- methods_lasso$lasso_type
#'   CATE_spec <- methods_lasso$CATE_spec
#'   
#'   train_data <- 
#'     preprocess_data(single_data, 
#'                     run_time_varying = T)
#'   
#'   n <- nrow(single_data)
#'   test_data <- 
#'     read_single_simulation_data(
#'       n = n, 
#'       i = i + 100, 
#'       eta_type = eta_type,
#'       CATE_type = CATE_type)$data
#'   
#'   T_lasso_ret <- 
#'     T_lasso(train_data = train_data, 
#'             test_data = test_data,
#'             lasso_type)
#'   
#'   
#'   # lasso_model <- 
#'   #   glmnet(transformed_X, Surv(tstart, tstop, Delta), 
#'   #          family = "cox",
#'   #          data = data_to_use)
#'   
#'   
#'   # 
#'   # cox_model <- 
#'   #   coxph(formula, 
#'   #         data = data_to_use, 
#'   #         ties = "breslow")
#'   # beta_est_cox <- extract_W_coefficients(
#'   #   fit=cox_model, 
#'   #   CATE_type = "constant")
#'   
#'   return(beta_est_cox)
#' }


#' Cox-based CATE Estimation with Flexible Regressor Specification
#'
#' This function fits a Cox proportional hazards regression model for estimating Conditional Average Treatment Effects (CATE) 
#' using flexible regressor specifications, including natural splines and interaction terms.
#'
#' @param train_data A data frame containing the training data. Must include columns for treatment (`W`), 
#'   time-varying covariates (`X.`), and survival times (`tstart`, `tstop`, `Delta`).
#' @param test_data A data frame containing the test data for evaluating the model. Must include columns 
#'   for CATE and survival-related variables.
#' @param regressor_spec A character string specifying the type of regressor transformation:
#'   - "linear-only": Uses linear terms only.
#'   - "mild-complex": Includes natural splines, square terms, and pairwise interactions for continuous variables.
#' @param CATE_spec A character string specifying the specification for CATE estimation:
#'   - "correctly-specified": Uses true variables (`X.1` and `X.10`) for CATE estimation.
#'   - "linear": Uses all variables starting with `X.` for CATE estimation with linear terms.
#'   - "flexible": Uses all transformed variables (splines, interactions, etc.) for flexible CATE estimation.
#' @param verbose A numeric flag controlling print statements for debugging (0 = no output, 1 = progress output, 2 = detailed output).
#'
#' @return A list containing the following components:
#'   - `m`: The fitted Cox model.
#'   - `m_beta`: The estimated coefficients from the model.
#'   - `beta_CATE`: Coefficients for CATE estimation.
#'   - `beta_eta_0`: Coefficients for baseline hazard estimation.
#'   - `y_0_pred`: Predicted baseline outcomes for the control group.
#'   - `y_1_pred`: Predicted outcomes for the treatment group.
#'   - `CATE_est`: Estimated CATE values.
#'   - `CATE_true`: True CATE values (from test data).
#'   - `MSE`: Mean squared error of CATE estimation.
#'
#' @examples
#' # Fit the model with "linear-only" regressor and "correctly-specified" CATE
#' result <- S_cox(train_data = df_train, test_data = df_test, regressor_spec = "linear-only", CATE_spec = "correctly-specified")
#'
#' @export
S_cox <- function(train_data, 
                  test_data, 
                  regressor_spec, 
                  CATE_spec, 
                  verbose = 0) {
  
  transformed_X <- transform_X(
    single_data = train_data, 
    regressor_spec = regressor_spec
  )
  
  if (CATE_spec == "correctly-specified") {
    regressor_CATE <- train_data$W * cbind(train_data$X.1, train_data$X.10)
  } else if (CATE_spec == "linear") {
    regressor_CATE <- cbind(train_data$W, train_data$W * train_data %>% select(starts_with("X.")))
  } else if (CATE_spec == "flexible") {
    regressor_CATE <- cbind(train_data$W, train_data$W * transformed_X)
  }
  
  regressor <- cbind(transformed_X, regressor_CATE)
  regressor <- as.matrix(regressor)
  
  # Fit Cox proportional hazards model instead of Lasso
  m <- coxph(Surv(train_data$tstart, train_data$tstop, train_data$Delta) ~ regressor, data = train_data)
  
  m_beta <- coef(m)  # Get the coefficients of the Cox model
  
  n_transformed_X <- ncol(transformed_X)
  beta_CATE <- m_beta[(n_transformed_X + 1):length(m_beta)] 
  beta_eta_0 <- m_beta[1:(n_transformed_X)] 
  
  if (verbose >= 1){
    print("Finished fitting the Cox model.")
  }
  
  test_transformed_X <- transform_X(
    single_data = test_data,
    regressor_spec = regressor_spec
  )
  
  if (verbose >= 1){
    print("Start prediction.")
  }
  
  if (CATE_spec == "correctly-specified") {
    test_regressor_CATE <- cbind(test_data$X.1, test_data$X.10)
  } else if (CATE_spec == "linear") {
    test_regressor_CATE <- cbind(1, as.matrix(test_data %>% select(starts_with("X."))))
  } else if (CATE_spec == "flexible") {
    test_regressor_CATE <- cbind(1, as.matrix(test_transformed_X))
  }
  
  CATE_est <- as.vector(test_regressor_CATE %*% beta_CATE)
  CATE_true <- test_data$CATE
  
  # Added y_0_pred
  y_0_pred <- as.vector(test_transformed_X %*% beta_eta_0)
  y_1_pred <- y_0_pred + CATE_est
  
  if (verbose >= 1){
    print("Finished prediction.")
  }
  
  MSE <- mean((CATE_est - CATE_true)^2)
  
  if (verbose == 2){
    print("MSE:")
    print(MSE)
  }
  
  ret <- list(
    m = m,
    m_beta = m_beta,
    beta_CATE = beta_CATE,
    beta_eta_0 = beta_eta_0,
    y_0_pred = y_0_pred,
    y_1_pred = y_1_pred, 
    CATE_est = CATE_est,
    CATE_true = CATE_true,
    MSE = MSE
  )
  
  if (verbose >= 1){
    print("m_beta:")
    print(m_beta)
    print("beta_CATE:")
    print(beta_CATE)
  }
  
  class(ret) <- "scox"
  ret
}



T_lasso <- function(train_data, 
                    test_data, 
                    regressor_spec = "mild-complex") {

  
  # Get row indexes for control and treatment groups
  index_co <- which(train_data$W == 0)
  index_tx <- which(train_data$W == 1)
  
  
  transformed_X <- transform_X(
    single_data = train_data, 
    regressor_spec = regressor_spec
  )
  # Separate transformed_X for control and treatment groups
  transformed_X_co <- transformed_X[index_co, ]
  transformed_X_tx <- transformed_X[index_tx, ]
  
  # Separate train_data for control and treatment groups
  data_co <- train_data[index_co, ]
  data_tx <- train_data[index_tx, ]
  
  # Fit Cox models for control and treatment groups
  eta_0 <- cv.glmnet(transformed_X_co, Surv(data_co$tstart, data_co$tstop, data_co$Delta), family = "cox")
  eta_1 <- cv.glmnet(transformed_X_tx, Surv(data_tx$tstart, data_tx$tstop, data_tx$Delta), family = "cox")
  
  print("eta_0 coefficient: ")
  print(coef(eta_0, s = "lambda.min"))
  print("eta_1 coefficient ")
  print(coef(eta_1, s = "lambda.min"))
  
  
  
  # Transform the test data
  test_transformed_X <- transform_X(
    single_data = test_data, 
    regressor_spec = regressor_spec
  )
  
  # Predict on test data using both models
  y_1_pred <- predict(eta_1, newx = test_transformed_X, s = "lambda.min")
  y_0_pred <- predict(eta_0, newx = test_transformed_X, s = "lambda.min")
  
  # print("head(y_1_pred)")
  # print(head(y_1_pred))
  # print("head(y_0_pred)")
  # print(head(y_0_pred))
  
  # Compute CATE estimate
  CATE_est <- y_1_pred - y_0_pred
  CATE_true <- test_data$CATE
  
  print("head(CATE_est)")
  print(head(CATE_est))
  print("head(CATE_true)")
  print(head(CATE_true))
  
  # Calculate Mean Squared Error
  MSE <- mean((CATE_est - CATE_true)^2)
  
  # Create and return the result list
  ret <- list(
    eta_0 = eta_0,
    eta_1 = eta_1,
    y_1_pred = y_1_pred,
    y_0_pred = y_0_pred,
    CATE_est = CATE_est,
    CATE_true = CATE_true,
    MSE = MSE
  )
  
  class(ret) <- "tlasso"
  return(ret)
}

#' Lasso-based CATE Estimation with Flexible Regressor Specification
#'
#' This function fits a Lasso Cox regression model for estimating Conditional Average Treatment Effects (CATE) 
#' using flexible regressor specifications, including natural splines and interaction terms.
#'
#' @param train_data A data frame containing the training data. Must include columns for treatment (`W`), 
#'   time-varying covariates (`X.`), and survival times (`tstart`, `tstop`, `Delta`).
#' @param test_data A data frame containing the test data for evaluating the model. Must include columns 
#'   for CATE and survival-related variables.
#' @param regressor_spec A character string specifying the type of regressor transformation:
#'   - "linear-only": Uses linear terms only.
#'   - "mild-complex": Includes natural splines, square terms, and pairwise interactions for continuous variables.
#' @param CATE_spec A character string specifying the specification for CATE estimation:
#'   - "correctly-specified": Uses true variables (`X.1` and `X.10`) for CATE estimation.
#'   - "linear": Uses all variables starting with `X.` for CATE estimation with linear terms.
#'   - "flexible": Uses all transformed variables (splines, interactions, etc.) for flexible CATE estimation.
#'
#' @return A list containing the following components:
#'   - `m`: The fitted Lasso Cox model.
#'   - `m_beta`: The estimated coefficients from the model.
#'   - `beta_CATE`: Coefficients for CATE estimation.
#'   - `beta_eta_0`: Coefficients for baseline hazard estimation.
#'   - `y_0_pred`: Predicted baseline outcomes for the control group.
#'   - `y_1_pred`: Predicted outcomes for the treatment group.
#'   - `CATE_est`: Estimated CATE values.
#'   - `CATE_true`: True CATE values (from test data).
#'   - `MSE`: Mean squared error of CATE estimation.
#'
#' @examples
#' # Fit the model with "linear-only" regressor and "correctly-specified" CATE
#' result <- S_lasso(train_data = df_train, test_data = df_test, regressor_spec = "linear-only", CATE_spec = "correctly-specified")
#'
#' @export
S_lasso <- function(train_data, 
                    test_data, 
                    regressor_spec, 
                    CATE_spec, 
                    verbose = 0) {
  
  transformed_X <- transform_X(
    single_data = train_data, 
    regressor_spec = regressor_spec
  )
  
  if (CATE_spec == "correctly-specified") {
    regressor_CATE <- train_data$W * cbind(train_data$X.1, train_data$X.10)
  } else if (CATE_spec == "linear") {
    # regressor_CATE <- cbind(train_data$W, train_data$W * train_data[, paste0("X.", 1:10)])
    regressor_CATE <- cbind(train_data$W, train_data$W * train_data %>% select(starts_with("X.")))
  } else if (CATE_spec == "flexible") {
    regressor_CATE <- cbind(train_data$W, train_data$W * transformed_X)
  }
  
  regressor <- cbind(transformed_X, regressor_CATE)
  regressor <- as.matrix(regressor)
  
  m <- cv.glmnet(regressor, Surv(train_data$tstart, train_data$tstop, train_data$Delta), 
                  # lambda = 10^seq(-4, 1, length = 100),
                 family = "cox"
                 )
  
  m_beta <- coef(m, s = "lambda.min")

  n_transformed_X <- ncol(transformed_X)
  beta_CATE <- m_beta[(n_transformed_X + 1):length(m_beta)] 
  beta_eta_0 <- m_beta[1:(n_transformed_X)] 
  
  if (verbose >= 1){
    print( "Finished fitting the lasso model. ")
  }
  
  
  test_transformed_X <- transform_X(
    single_data = test_data,
    regressor_spec = regressor_spec)
  
  
  if (verbose >= 1){
    print( "Start prediction. ")
  }
  
  if (CATE_spec == "correctly-specified") {
    test_regressor_CATE <- cbind(test_data$X.1, test_data$X.10)
  } else if (CATE_spec == "linear") {
    test_regressor_CATE <- cbind(1, as.matrix(test_data %>% select(starts_with("X.")) ) )
    # test_regressor_CATE <- cbind(1, as.matrix(test_data[, paste0("X.", 1:10)]))
  } else if (CATE_spec == "flexible") {
    test_regressor_CATE <- cbind(1, as.matrix(test_transformed_X))
  }
  
  CATE_est <- as.vector(test_regressor_CATE %*% beta_CATE)
  CATE_true <- test_data$CATE
  
  # Added y_0_pred
  y_0_pred <- as.vector(test_transformed_X %*% beta_eta_0)
  y_1_pred <- y_0_pred + CATE_est
  
  if (verbose >= 1){
    print( "Finished prediction. ")
  }
  
  
  MSE <- mean((CATE_est - CATE_true)^2)
  if (verbose == 2){
    print( "MSE: ")
    print( MSE )
  }
  
  ret <- list(
    m = m,
    m_beta = m_beta,
    beta_CATE = beta_CATE,
    beta_eta_0 = beta_eta_0,
    y_0_pred = y_0_pred,
    y_1_pred = y_1_pred, 
    CATE_est = CATE_est,
    CATE_true = CATE_true,
    MSE = MSE
  )
  
  print("m_beta: ")
  print(m_beta)
  print("beta_CATE: ")
  print(beta_CATE)
  
  class(ret) <- "slasso"
  ret
}

#' Time-Varying Conditional Survival Learning (TV-CSL) with K-Fold Cross-Fitting
#'
#' This function performs K-fold cross-fitting to estimate the Conditional Average Treatment Effect (CATE) using 
#' Time-Varying Conditional Survival Learning (TV-CSL). The procedure includes fitting nuisance parameters 
#' and the final model on different folds of the data.
#'
#' @param train_data A data frame containing the training data. Must include variables needed for model training and cross-fitting, including an `id` column.
#' @param test_data A data frame containing the test data. Must include the true CATE values (`CATE`).
#' @param train_data_original A data frame with the original training data used for model fitting.
#' @param folds A vector or factor specifying the fold assignment for each observation in `train_data`.
#' @param K An integer specifying the number of folds for cross-fitting.
#' @param prop_score_spec A character string specifying the type of propensity score specification to use in the nuisance model.
#' @param lasso_type A character string specifying the type of Lasso to use in the model (e.g., "linear", "mild-complex").
#' @param regressor_spec A character string specifying the regressor transformation ("linear-only" or "mild-complex").
#' @param final_model_method A character string specifying the final model method (e.g., "cox", "lasso").
#'
#' @return A list containing the following components:
#'   - `CATE_est`: Estimated CATE values for the test data.
#'   - `CATE_true`: True CATE values from the test data.
#'   - `MSE`: Mean squared error between the estimated and true CATE.
#'   - `time_taken`: Time taken to run the K-fold cross-fitting procedure.
#'
#' @examples
#' # Example usage of TV_CSL function
#' result <- TV_CSL(
#'   train_data = df_train, 
#'   test_data = df_test, 
#'   train_data_original = df_train_original,
#'   folds = folds_vector,
#'   K = 5, 
#'   prop_score_spec = "logit",
#'   lasso_type = "mild-complex",
#'   regressor_spec = "linear-only",
#'   final_model_method = "cox"
#' )
#'
#' @export
TV_CSL <- function(train_data, 
                   test_data, 
                   train_data_original, 
                   K, 
                   prop_score_spec, 
                   lasso_type, 
                   regressor_spec, 
                   final_model_method,
                   id_var = "id") {
  
  n <- nrow(test_data)
  folds <- cut(seq(1, nrow(train_data_original)), breaks = K, labels = FALSE)
  CATE_ests <- matrix(NA, nrow = n, ncol = K)
  
  # Measure time taken for the procedure
  start_time <- Sys.time()
  
  {
    train_data <- train_data %>% mutate(id = !!sym(id_var))
    test_data <- test_data %>% mutate(id = !!sym(id_var))
    train_data_original <- train_data_original %>% mutate(id = !!sym(id_var))
  }
  
  
  
  # Perform K-fold cross-fitting
  for (k in 1:K) {
    
    # Get IDs for nuisance and causal splits
    nuisance_ids <- train_data_original[folds != k, id_var]
    causal_ids <- train_data_original[folds == k, id_var]
    
    # Split the training data
    fold_nuisance <- train_data[train_data[[id_var]] %in% nuisance_ids, ]
    fold_causal <- train_data[train_data[[id_var]] %in% causal_ids, ]
    train_data_original_nuisance <- 
      train_data_original[train_data_original[[id_var]] %in% nuisance_ids, ]
    
    
    fold_causal_fitted <- TV_CSL_nuisance(
      fold_train = fold_nuisance, 
      fold_test = fold_causal, 
      train_data_original = train_data_original_nuisance,
      prop_score_spec = prop_score_spec,
      lasso_type = lasso_type,
      regressor_spec = regressor_spec
    )
    
    # Fit final model and estimate CATE
    fit_TV_CSL_ret <- fit_TV_CSL(
      fold_causal_fitted = fold_causal_fitted, 
      final_model_method = final_model_method,
      test_data = test_data
    )
    
    # Store CATE estimates for the k-th fold
    CATE_ests[, k] <- fit_TV_CSL_ret$CATE_est
  }
  
  # Average CATE estimates across folds
  CATE_est <- rowMeans(CATE_ests, na.rm = TRUE)
  CATE_true <- test_data$CATE
  
  # Calculate mean squared error (MSE)
  MSE <- mean((CATE_true - CATE_est)^2)
  
  # Time taken for the cross-fitting process
  time_taken <- Sys.time() - start_time
  
  # Return results as a list
  return(list(
    CATE_est = CATE_est,
    CATE_true = CATE_true,
    MSE = MSE,
    time_taken = time_taken
  ))
}



#' Transform X Variables for Lasso Regression
#'
#' This function performs transformations on variables starting with "X." from the input data.
#' Depending on the `regressor_spec`, the function creates either simple linear terms or a more complex set of transformations including natural splines, square terms, and interaction terms.
#'
#' @param single_data A data frame that contains the regressors, with column names starting with "X.".
#' @param regressor_spec A character string specifying the type of transformation. 
#'   - "linear-only": Generates linear terms only.
#'   - "mild-complex": Generates linear terms, 3 natural splines for continuous variables, square terms, and pairwise interaction terms for both continuous and binary variables.
#'
#' @details
#' - If `regressor_spec` is "linear-only", the function will return a matrix of linear terms from all variables starting with "X.".
#' - If `regressor_spec` is "mild-complex", the function applies the following transformations:
#'   - Linear terms for all variables.
#'   - 3 natural spline terms for continuous variables.
#'   - Square terms for continuous variables.
#'   - Pairwise interaction terms between all variables (both continuous and binary).
#'
#' @return A matrix of transformed regressors, including linear terms, splines, square terms, and interactions depending on the specified transformation type.
#' 
#' @examples
#' # Using linear-only transformation
#' transformed_X_linear <- transform_X(single_data = df_time_var, regressor_spec = "linear-only")
#'
#' # Using mild-complex transformation
#' transformed_X_complex <- transform_X(single_data = df_time_var, regressor_spec = "mild-complex")
#'
#' @export
transform_X <- function(single_data, regressor_spec = "linear-only") {
  library(splines)
  library(dplyr)
  
  X_vars <- single_data %>% select(starts_with("X."))
  # X_vars <- single_data[grep("^X", names(single_data))]
  
  transformed_X <- matrix(nrow = nrow(X_vars), ncol = 0)
  
  if (regressor_spec == "mild-complex") {
    
    # transformed_X <- as.matrix(X_vars)
    continuous_vars <- names(X_vars)[sapply(X_vars, function(x) length(unique(x)) > 2)]
    
    # Apply natural splines to continuous variables
    for (var_name in continuous_vars) {
      spline_terms <- ns(X_vars[[var_name]], df = 3)
      colnames(spline_terms) <- paste0(var_name, "_spline", 1:3)
      transformed_X <- cbind(transformed_X, as.matrix(spline_terms))
    }
    
    # Add square terms for continuous variables
    for (var_name in continuous_vars) {
      square_term <- X_vars[[var_name]]^2
      transformed_X <- cbind(transformed_X, square_term)
      colnames(transformed_X)[ncol(transformed_X)] <- paste0(var_name, "_squared")
    }
    
    # Add interaction terms between all pairs of variables (both continuous and binary)
    var_names <- names(X_vars)
    for (i in 1:(length(var_names) - 1)) {
      for (j in (i + 1):length(var_names)) {
        interaction_term <- X_vars[[var_names[i]]] * X_vars[[var_names[j]]]
        transformed_X <- cbind(transformed_X, interaction_term)
        colnames(transformed_X)[ncol(transformed_X)] <- paste0(var_names[i], "_x_", var_names[j])
      }
    }
    
  } else if (regressor_spec == "linear-only") {
    
    transformed_X <- as.matrix(X_vars)
    
  }
  
  return(transformed_X)
}



#' Run Cox Model Estimation for Different Specifications
#'
#' This function runs Cox model estimation for the provided dataset 
#' using different model specifications from the \code{methods_cox}.
#'
#' @param single_data A data frame containing the survival data.
#' @param methods_cox A list containing the model specification and time-varying flag for Cox model.
#' @return A list of results containing the tau estimates and time taken for each model specification.
#' @export
run_cox_estimation <- function(
    single_data, methods_cox, CATE_type, eta_type) {
  results <- list()
  
  if (methods_cox$enabled) {
    for (spec in methods_cox$model_specifications) {
      for (run_time_varying in methods_cox$run_time_varying) {
        
        config_name <- paste(spec, run_time_varying, sep = "_")
        
        start_time <- Sys.time()
        
        # Create a temporary methods_cox object that contains the current specification and run_time_varying
        current_methods_cox <- methods_cox
        current_methods_cox$model_spec <- spec
        current_methods_cox$run_time_varying <- run_time_varying
        
        # Run the Cox model estimation using the current configuration
        # tau_est_cox <- 
        beta_est_cox <- 
          cox_model_estimation(
            single_data = single_data, 
            methods_cox = current_methods_cox,
            CATE_type = CATE_type,
            eta_type = eta_type
          )
        
        end_time <- Sys.time()
        
        time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        results[[config_name]] <- list(
          # tau_estimate = tau_est_cox,
          beta_estimate = beta_est_cox,
          time_taken = time_taken
        )
      }
    }
  }
  
  return(results)
}


#'
#' This function runs lasso model estimation for the provided dataset 
#' using different model specifications from the \code{methods_lasso}.
#'
#' @param single_data A data frame containing the survival data.
#' @param methods_cox A list containing the model specification and time-varying flag for Cox model.
#' @return A list of results containing the tau estimates and time taken for each model specification.
#' @export
run_lasso_estimation <- function(
    single_data, i, methods_lasso, CATE_type, eta_type) {
  results <- list()
  n <- nrow(single_data)
  test_data <- 
    read_single_simulation_data(
      n = n, 
      i = i + 100, 
      eta_type = eta_type,
      CATE_type = CATE_type)$data
  train_data <- 
    preprocess_data(single_data, 
                    run_time_varying = T)
  
    for (regressor_spec in methods_lasso$regressor_specs) {
      for (lasso_type in methods_lasso$lasso_types) {
        
        
        
        if (lasso_type == "T-lasso"){
          config_name <- paste(lasso_type, regressor_spec, sep = "_")
          start_time <- Sys.time()
          lasso_ret <-
            T_lasso(train_data = train_data,
                    test_data = test_data,
                    regressor_spec = regressor_spec)
          ## Test output
          # lasso_ret <- list(MSE = 1, 
          #                   CATE_est = 1,
          #                   CATE_true = 1)
          
          end_time <- Sys.time()
          
          
          time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          results[[config_name]] <- list(
            CATE_est = lasso_ret$CATE_est,
            CATE_true = lasso_ret$CATE_true,
            MSE = lasso_ret$MSE,
            time_taken = time_taken
          )
          print(paste0("config_name: ", config_name, ". MSE: ", lasso_ret$MSE, ". time_taken: ", time_taken))
          
        }else if (lasso_type == "S-lasso"){
          for (CATE_spec in methods_lasso$CATE_specs){
            print(paste0("CATE_spec: ", CATE_spec))
            
            start_time <- Sys.time()
            config_name <- paste(lasso_type, CATE_spec, regressor_spec, sep = "_")
            lasso_ret <-
              S_lasso(train_data = train_data,
                      test_data = test_data,
                      regressor_spec = regressor_spec,
                      CATE_spec = CATE_spec)
            ## Test output
            # lasso_ret <- list(MSE = 1, 
            #                   CATE_est = 1,
            #                   CATE_true = 1)
            
            end_time <- Sys.time()
            
            time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
            print(paste0("config_name: ", config_name, "; MSE: ", lasso_ret$MSE, "; time_taken: ", time_taken))
            
            results[[config_name]] <- list(
              CATE_est = lasso_ret$CATE_est,
              CATE_true = lasso_ret$CATE_true,
              MSE = lasso_ret$MSE,
              time_taken = time_taken
            )
          }
        }else{
          stop("lasso_type should be T_lasso or S_lasso")
        }
        
      } # End looping lasso_types
    } # End looping regressor_specs
  
  return(results)
}



#' Run TV_CSL Estimation
#'
#' This function runs the Time-Varying Conditional Survival Learning (TV-CSL) model estimation with multiple configurations. It performs k-fold cross-validation for different specifications of the model, including the type of lasso, regressor, and propensity score method.
#'
#' @param train_data_original A data frame containing the original training data.
#' @param test_data A data frame containing the test data.
#' @param methods_TV_CSL A list specifying the various configurations to use in TV-CSL, including the propensity score specification, lasso types, and final model methods.
#' @param K An integer representing the number of folds for cross-validation. Default is 5.
#'
#' @details
#' The function takes the training and test data, and for each configuration of propensity score specification, lasso type, regressor specification, and final model method, it performs k-fold cross-validation. It estimates the Conditional Average Treatment Effect (CATE) for each fold and returns the results, including the MSE and computation time for each configuration.
#'
#' @return A list where each entry corresponds to a configuration and contains the CATE estimates, true CATE values, MSE, and the time taken.
#'
#' @examples
#' \dontrun{
#' # Example of how to run the function
#' results <- run_TV_CSL_estimation(train_data_original, test_data, methods_TV_CSL, K = 5)
#' }
#'
#' @export
run_TV_CSL_estimation <- function(
    train_data_original, 
    test_data,
    methods_TV_CSL,
    K,
    temp_result_csv_file
) {
  
  results <- list()
  n <- nrow(train_data_original)
  
  train_data <- create_pseudo_dataset(survival_data = train_data_original)
  train_data_original <- train_data_original %>% 
    mutate(U_A = pmin(A,U),
           Delta_A = A <= U)
  
  for (prop_score_spec in methods_TV_CSL$prop_score_specs) {
    for (regressor_spec in methods_TV_CSL$regressor_specs) {
      for (lasso_type in methods_TV_CSL$lasso_types) {
        for (final_model_method in methods_TV_CSL$final_model_methods) {
          
          config_name <- paste(lasso_type, prop_score_spec, regressor_spec, final_model_method, sep = "_")
          start_time <- Sys.time()
          
          TV_CSL_ret <- TV_CSL(train_data, 
                               test_data, 
                               train_data_original, 
                               K, 
                               prop_score_spec, 
                               lasso_type, 
                               regressor_spec, 
                               final_model_method) 
          
          # CATE_ests <- matrix(NA, nrow = n, ncol = K)
          # 
          # # Perform K-fold cross-fitting
          # for (k in 1:K) {
          #   
          #   nuisance_ids <- train_data_original[folds != k, "id"]
          #   causal_ids <- train_data_original[folds == k, "id"]
          #   
          #   fold_nuisance <- train_data[train_data$id %in% nuisance_ids, ]
          #   fold_causal <- train_data[train_data$id %in% causal_ids, ]
          #   train_data_original_nuisance <- 
          #     train_data_original[train_data_original$id %in% nuisance_ids, ]
          #   
          #   fold_causal_fitted <- TV_CSL_nuisance(
          #     fold_train = fold_nuisance, 
          #     fold_test = fold_causal, 
          #     train_data_original = train_data_original_nuisance,
          #     prop_score_spec = prop_score_spec,
          #     lasso_type = lasso_type,
          #     regressor_spec = regressor_spec
          #   )
          #   
          #   fit_TV_CSL_ret <- fit_TV_CSL(
          #     fold_causal_fitted = fold_causal_fitted, 
          #     final_model_method = final_model_method,
          #     test_data = test_data
          #   )
          #   
          #   CATE_ests[, k] <- fit_TV_CSL_ret$CATE_est
          # }
          # 
          # end_time <- Sys.time()
          # 
          # CATE_est <- rowMeans(CATE_ests, na.rm = TRUE)
          # CATE_true <- test_data$CATE
          # 
          # MSE <- mean((CATE_true - CATE_est)^2)
          
          time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          results[[config_name]] <- TV_CSL_ret 
          # results[[config_name]] <- list(
          #   CATE_est = CATE_est,
          #   CATE_true = CATE_true,
          #   MSE = MSE,
          #   time_taken = time_taken
          # )
          
          print(paste0("config_name: ", config_name, ". MSE: ", MSE, ". time_taken: ", time_taken))
          
          
          library(readr)
          save_res_to_csv<-
            function(curr_res,
                     FNAME){
              curr_res_df <- as.data.frame(curr_res, stringsAsFactors = FALSE)
              
              if (file.exists(FNAME)) {
                write_csv(curr_res_df, FNAME, append=TRUE)
              } else {
                write_csv(curr_res_df, FNAME)
              }
              print(paste("Result for config_name:", curr_res_df$config_name, "saved to", FNAME))
            } 
          curr_res <- list(
            Method = "TV_CSL",
            config_name = config_name,
            MSE = MSE,
            time_taken = time_taken
          )
          save_res_to_csv(curr_res, FNAME = temp_result_csv_file)
          
          
        } # End looping over final_model_methods
      } # End looping over lasso_types
    } # End looping over regressor_specs
  } # End looping over prop_score_specs
  
  return(results)
}


calculate_eX <- function(alpha_estimate, X, t) {
  hazard_adapt_date <- exp(X %*% alpha_estimate)
  trt_prob <- 1 - exp(-t * hazard_adapt_date)
  return(trt_prob)
}

#' TV_CSL Nuisance Estimation
#'
#' This function estimates the nuisance components for the time-varying conditional survival model (TV-CSL). It calculates the propensity score and performs the lasso-based nuisance estimation.
#'
#' @param fold_train A data frame containing the training set.
#' @param fold_test A data frame containing the test set.
#' @param prop_score_spec A character string specifying the method for propensity score estimation (e.g., "cox-linear-censored-only").
#' @param lasso_type A character string specifying the type of lasso model ("T-lasso" or "S-lasso").
#' @param regressor_spec A character string specifying the regressor used in the lasso model.
#'
#' @details
#' The function first estimates the propensity score based on the specified propensity score model. If the model is based on the Cox proportional hazards model (e.g., "cox-linear-censored-only"), it estimates the treatment probability using a Cox regression. It then splits the test set based on time intervals, computes the treatment probabilities within each interval, and fits a lasso model (either "T-lasso" or "S-lasso") to estimate the nuisance parameters for each fold.
#'
#' @return A data frame containing the augmented test set, including the nuisance estimates.
#'
#' @examples
#' # Example usage of TV_CSL_nuisance
#' fold_train <- your_train_data
#' fold_test <- your_test_data
#' result <- TV_CSL_nuisance(fold_train, fold_test, "cox-linear-censored-only", "T-lasso", "linear")
#'
#' @importFrom dplyr mutate left_join filter
#' @importFrom survival coxph Surv
#' @importFrom glmnet cv.glmnet
#' @export
TV_CSL_nuisance <- function(fold_train, 
                            fold_test,
                            train_data_original, # for estimating the propensity score
                            prop_score_spec,
                            lasso_type,
                            regressor_spec,
                            id_var = "id") {
  
  # 1. Estimate the propensity score
  if (grepl("^cox", prop_score_spec)) {
    if (prop_score_spec == "cox-linear-censored-only") {
      df_prop_score <- train_data_original %>% filter(Delta == 1)
    }else if (prop_score_spec == "cox-linear-all-data") {
      df_prop_score <- train_data_original
    }
    
    treatment_model <- coxph(Surv(U_A, Delta_A) ~ X.1 + X.2 + X.3, 
                             data = df_prop_score, 
                             ties = "breslow")
    alpha_estimate <- treatment_model$coefficients
    
  }else {
    stop(paste0("Unknown prop_score_spec."))
  }
  
  # 2. Obtain the propensity score at each interval
  granular_cut_points <- unique(fold_test$tstop)
  
  split_within_intervals <- function(row, cut_points) {
    original_tstart <- row[["tstart"]]
    original_tstop <- row[["tstop"]]
    
    valid_cuts <- cut_points[cut_points > original_tstart & cut_points < original_tstop]
    final_cuts <- sort(c(original_tstart, valid_cuts, original_tstop))
    
    new_intervals <- data.frame(
      tstart = head(final_cuts, -1),
      tstop = tail(final_cuts, -1),
      id = row[["id"]],
      W = row[["W"]]
    )
    return(new_intervals)
  }
  
  fold_test_split <- map_dfr(seq_len(nrow(fold_test)), function(i) {
    split_within_intervals(fold_test[i, ], granular_cut_points)
  })
  
  
  
  # 3. Obtain nu 
  if (lasso_type == "T-lasso") {
    lasso_ret <- T_lasso(
      train_data = fold_train,  
      test_data = fold_test,  
      regressor_spec = regressor_spec
    )
  } else if (lasso_type == "S-lasso") {
    lasso_ret <- S_lasso(
      train_data = fold_train,
      test_data = fold_test,
      regressor_spec = regressor_spec,
      CATE_spec = "linear" # we hard code this
    )
    # fold_test$nu_X <- lasso_ret$y_pred
  }
  fold_test$eta_1 <- lasso_ret$y_1_pred
  fold_test$eta_0 <- lasso_ret$y_0_pred
  
  # 3.2 Join the data
  fold_test_final <- 
    left_join(fold_test_split, 
              fold_test %>% select(-tstart, -tstop), 
              by = c("id", "W") )
  # print("colnames(fold_test_split)")
  # print(colnames(fold_test_split))
  # print("colnames(fold_test %>% select(-tstart, -tstop))")
  # print(colnames(fold_test %>% select(-tstart, -tstop)))
  # 
  # print(head(fold_test_final))
  print(paste("alpha_estimate: ", alpha_estimate))
  test_X <- cbind(fold_test_final$X.1, fold_test_final$X.2, fold_test_final$X.3)
  prop_scores <- calculate_eX(
    alpha_estimate = alpha_estimate, 
    X = test_X,
    t = fold_test_split$tstop
  )
  print("head(prop_scores):")
  print( head(prop_scores) )
  
  fold_test_final <- fold_test_final %>%
    mutate(a_t_X = prop_scores)
  
  fold_test_final <- fold_test_final %>%
    mutate(nu_X = a_t_X * eta_1 + (1 - a_t_X) * eta_0)
  
  return(fold_test_final)
}


fit_TV_CSL <- function(fold_causal_fitted, final_model_method, test_data) {
  
  beta_CATE <- NULL
  
  if (final_model_method == "coxph") {
    
    # Select regressors that start with "X."
    regressors <- fold_causal_fitted %>% select(starts_with("X."))
    n_regressors <- ncol(regressors)  # Get the number of regressors
    
    # Compute interaction terms between (W - a_t_X) and the regressors
    interaction_terms <- (fold_causal_fitted$W - fold_causal_fitted$a_t_X) * regressors
    
    # Name the new interaction columns dynamically (interaction_X1, interaction_X2, ..., interaction_Xn)
    colnames(interaction_terms) <- paste0("interaction_X", 1:n_regressors)
    
    # Add interaction terms to the data frame
    fold_causal_fitted <- cbind(fold_causal_fitted, interaction_terms)
    
    # Create the formula dynamically using paste()
    interaction_formula <- paste(paste0("interaction_X", 1:n_regressors), collapse = " + ")
    final_formula <- as.formula(paste("Surv(tstart, tstop, Delta) ~", interaction_formula, "+ offset(nu_X)"))
    
    # Fit the Cox proportional hazards model using the dynamic formula
    final_model <- coxph(
      final_formula, 
      data = fold_causal_fitted, 
      ties = "breslow"
    )
    
    # Extract the coefficients
    beta_CATE <- coef(final_model)
  } else if (final_model_method == "lasso") {
    
    regressor_TV_CSL <- (fold_causal_fitted$W - as.vector(fold_causal_fitted$a_t_X)) *
      as.matrix(fold_causal_fitted[, paste0("X.", 1:10)])
    
    final_model <- cv.glmnet(
      regressor_TV_CSL, 
      Surv(fold_causal_fitted$tstart, fold_causal_fitted$tstop, fold_causal_fitted$Delta), 
      offset = fold_causal_fitted$nu_X,
      family = "cox"
    )

    beta_CATE <- as.vector(coef(final_model, s = "lambda.min"))  
    
  }
  print("beta_CATE")
  print(beta_CATE)
  test_regressor_CATE <- as.matrix(test_data %>% select(starts_with("X.")))
  CATE_est <- as.vector(test_regressor_CATE %*% beta_CATE)
  
  ret <- list(
    CATE_est = CATE_est,
    beta_CATE = beta_CATE,
    final_model_method = final_model_method
  )
  
  return(ret)
}
