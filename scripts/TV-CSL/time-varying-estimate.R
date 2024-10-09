
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

S_lasso <- function(train_data, test_data, regressor_spec, CATE_spec) {
  
  transformed_X <- transform_X(
    single_data = train_data, 
    regressor_spec = regressor_spec
  )
  
  if (CATE_spec == "correctly-specified") {
    regressor_CATE <- train_data$W * cbind(train_data$X.1, train_data$X.10)
  } else if (CATE_spec == "linear") {
    regressor_CATE <- cbind(train_data$W, train_data$W * train_data[, paste0("X.", 1:10)])
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
  
  # CATE_est_train <- as.vector(cbind(train_data$X.1, train_data$X.10) %*% beta_CATE)
  # MSE_train <- mean( (CATE_est_train - train_data$CATE)^2)
  
  test_transformed_X <- transform_X(
    single_data = test_data,
    regressor_spec = regressor_spec)
  
  
  if (CATE_spec == "correctly-specified") {
    test_regressor_CATE <- cbind(test_data$X.1, test_data$X.10)
  } else if (CATE_spec == "linear") {
    test_regressor_CATE <- cbind(1, as.matrix(test_data[, paste0("X.", 1:10)]))
  } else if (CATE_spec == "flexible") {
    test_regressor_CATE <- cbind(1, as.matrix(test_transformed_X))
  }
  
  CATE_est <- as.vector(test_regressor_CATE %*% beta_CATE)
  CATE_true <- test_data$CATE
  
  # Added y_0_pred
  # print("head(test_regressor): ")
  # print(head(test_regressor))
  # print("m_beta: ")
  # print(m_beta)
  y_0_pred <- as.vector(test_transformed_X %*% beta_eta_0)
  y_1_pred <- y_0_pred + CATE_est
  
  
  
  
  MSE <- mean((CATE_est - CATE_true)^2)
  print(MSE)
  
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



# Transform X: have a transformation function that takes into single_data
# 1. extract all variables started with "X."
# 2. perform transformation
# lasso_type == “mild-complex”: 
#   - Linear terms and 3 natural splines 
# - Square of the linear terms
# - Pairwise interactions of the linear terms
# 
# lasso_type == “linear-only”:
#   - Linear terms
transform_X <- function(single_data, regressor_spec = "linear-only") {
  library(splines)
  X_vars <- single_data[grep("^X", names(single_data))]
  
  transformed_X <- matrix(nrow = nrow(X_vars), ncol = 0)
  
  if (regressor_spec == "mild-complex") {
    
    transformed_X <- as.matrix(X_vars)
    
    for (var_name in names(X_vars)) {
      spline_terms <- ns(X_vars[[var_name]], df = 3)
      colnames(spline_terms) <- paste0(var_name, "_spline", 1:3)
      transformed_X <- cbind(transformed_X, as.matrix(spline_terms))
    }
    
    for (var_name in names(X_vars)) {
      square_term <- X_vars[[var_name]]^2
      transformed_X <- cbind(transformed_X, as.matrix(square_term))
    }
    
    var_names <- names(X_vars)
    for (i in 1:(length(var_names)-1)) {
      for (j in (i+1):length(var_names)) {
        interaction_term <- X_vars[[var_names[i]]] * X_vars[[var_names[j]]]
        transformed_X <- cbind(transformed_X, as.matrix(interaction_term))
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
        
        
        
        if (lasso_type == "T_lasso"){
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
          
        }else if (lasso_type == "S_lasso"){
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
    K = 5
) {
  
  results <- list()
  n <- nrow(train_data_original)
  
  train_data <- create_pseudo_dataset(survival_data = train_data_original)
  train_data_original <- train_data_original %>% 
    mutate(U_A = pmin(A,U),
           Delta_A = A <= U)
  
  folds <- cut(seq(1, n), breaks = K, labels = FALSE)
  
  for (prop_score_spec in methods_TV_CSL$prop_score_specs) {
    for (regressor_spec in methods_TV_CSL$regressor_specs) {
      for (lasso_type in methods_TV_CSL$lasso_types) {
        for (final_model_method in methods_TV_CSL$final_model_methods) {
          
          config_name <- paste(lasso_type, prop_score_spec, regressor_spec, final_model_method, sep = "_")
          start_time <- Sys.time()
          
          CATE_ests <- matrix(NA, nrow = n, ncol = K)
          
          # Perform K-fold cross-fitting
          for (k in 1:K) {
            
            nuisance_ids <- train_data_original[folds != k, "id"]
            causal_ids <- train_data_original[folds == k, "id"]
            
            fold_nuisance <- train_data[train_data$id %in% nuisance_ids, ]
            fold_causal <- train_data[train_data$id %in% causal_ids, ]
            train_data_original_nuisance <- 
              train_data_original[train_data_original$id %in% nuisance_ids, ]
            
            fold_causal_fitted <- TV_CSL_nuisance(
              fold_train = fold_nuisance, 
              fold_test = fold_causal, 
              train_data_original = train_data_original_nuisance,
              prop_score_spec = prop_score_spec,
              lasso_type = lasso_type,
              regressor_spec = regressor_spec
            )
            
            fit_TV_CSL_ret <- fit_TV_CSL(
              fold_causal_fitted = fold_causal_fitted, 
              final_model_method = final_model_method,
              test_data = test_data
            )
            
            CATE_ests[, k] <- fit_TV_CSL_ret$CATE_est
          }
          
          end_time <- Sys.time()
          
          CATE_est <- rowMeans(CATE_ests, na.rm = TRUE)
          CATE_true <- test_data$CATE
          
          MSE <- mean((CATE_true - CATE_est)^2)
          
          time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          results[[config_name]] <- list(
            CATE_est = CATE_est,
            CATE_true = CATE_true,
            MSE = MSE,
            time_taken = time_taken
          )
          
          print(paste0("config_name: ", config_name, ". MSE: ", MSE, ". time_taken: ", time_taken))
          
        } # End looping over final_model_methods
      } # End looping over lasso_types
    } # End looping over regressor_specs
  } # End looping over prop_score_specs
  
  return(results)
}



#' TV_CSL Nuisance Estimation
#'
#' This function estimates the nuisance components for the time-varying conditional survival model (TV-CSL). It calculates the propensity score and performs the lasso-based nuisance estimation.
#'
#' @param fold_train A data frame containing the training set.
#' @param fold_test A data frame containing the test set.
#' @param prop_score_spec A character string specifying the method for propensity score estimation (e.g., "cox-linear-censored-only").
#' @param lasso_type A character string specifying the type of lasso model ("T_lasso" or "S_lasso").
#' @param regressor_spec A character string specifying the regressor used in the lasso model.
#'
#' @details
#' The function first estimates the propensity score based on the specified propensity score model. If the model is based on the Cox proportional hazards model (e.g., "cox-linear-censored-only"), it estimates the treatment probability using a Cox regression. It then splits the test set based on time intervals, computes the treatment probabilities within each interval, and fits a lasso model (either "T_lasso" or "S_lasso") to estimate the nuisance parameters for each fold.
#'
#' @return A data frame containing the augmented test set, including the nuisance estimates.
#'
#' @examples
#' # Example usage of TV_CSL_nuisance
#' fold_train <- your_train_data
#' fold_test <- your_test_data
#' result <- TV_CSL_nuisance(fold_train, fold_test, "cox-linear-censored-only", "T_lasso", "linear")
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
                            regressor_spec) {
  
  # 1. Estimate the propensity score
  if (grepl("^cox", prop_score_spec)) {
    if (prop_score_spec == "cox-linear-censored-only") {
      df_prop_score <- train_data_original %>% filter(Delta == 1)
    }else if (prop_score_spec == "cox-linear-all-data") {
      df_prop_score <- train_data_original
    }
    
    treatment_model <- coxph(Surv(U_A, Delta_A) ~ X.2 + X.3, 
                             data = df_prop_score, 
                             ties = "breslow")
    alpha_estimate <- treatment_model$coefficients
    
    calculate_eX <- function(alpha_estimate, X, t) {
      hazard_adapt_date <- exp(alpha_estimate * X)
      trt_prob <- 1 - exp(-t * hazard_adapt_date)
      return(trt_prob)
    }
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
      id = row[["id"]]
    )
    return(new_intervals)
  }
  
  fold_test_split <- map_dfr(seq_len(nrow(fold_test)), function(i) {
    split_within_intervals(fold_test[i, ], granular_cut_points)
  })
  
  fold_test_split <- fold_test_split %>%
    mutate(a_t_X = calculate_eX(
      alpha_estimate = alpha_estimate, 
      X = cbind(fold_test_split$X.2, fold_test_split$X.3),
      t = fold_test_split$tstop
    ))
  
  # 3. Obtain nu 
  if (lasso_type == "T_lasso") {
    lasso_ret <- T_lasso(
      train_data = fold_train,  
      test_data = fold_test,  
      regressor_spec = regressor_spec
    )
  } else if (lasso_type == "S_lasso") {
    lasso_ret <- S_lasso(
      train_data = fold_train,
      test_data = fold_test,
      regressor_spec = regressor_spec,
      CATE_spec = CATE_spec
    )
    # fold_test$nu_X <- lasso_ret$y_pred
  }
  fold_test$eta_1 <- lasso_ret$y_1_pred
  fold_test$eta_0 <- lasso_ret$y_0_pred
  
  # 3.2 Join the data
  fold_test_final <- 
    left_join(fold_test_split, 
              fold_test %>% select(-tstart, -tstop), 
              by = "id")
  
  fold_test_final <- fold_test_final %>%
    mutate(nu_X = a_t_X * eta_1 + (1 - a_t_X) * eta_0)
  
  return(fold_test_final)
}


fit_TV_CSL <- function(fold_causal_fitted, final_model_method, test_data) {
  
  beta_CATE <- NULL
  
  if (final_model_method == "coxph") {
    final_model <- coxph(
      Surv(tstart, tstop, Delta) ~ I(W - a_t_X) * paste0("X.", 1:10) + offset(nu_X), 
      data = fold_causal_fitted, 
      ties = "breslow"
    )
    beta_CATE <- coef(final_model)
    
  } else if (final_model_method == "lasso") {
    regressor_TV_CSL <- (fold_causal_fitted$W - fold_causal_fitted$a_t_X) *
      cbind(1, fold_causal_fitted[, paste0("X.", 1:10)])
    
    final_model <- cv.glmnet(
      regressor_TV_CSL, 
      Surv(fold_causal_fitted$tstart, fold_causal_fitted$tstop, fold_causal_fitted$Delta), 
      offset = fold_causal_fitted$nu_X,
      family = "cox"
    )

    beta_CATE <- as.vector(coef(final_model, s = "lambda.min")[-1])  # Removing the intercept
  }
  
  test_regressor_CATE <- cbind(1, test_data[, paste0("X.", 1:10)])
  CATE_est <- as.vector(test_regressor_CATE %*% beta_CATE)
  
  ret <- list(
    CATE_est = CATE_est,
    beta_CATE = beta_CATE,
    final_model_method = final_model_method
  )
  
  return(ret)
}
