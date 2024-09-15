get_ns_features <- function(X){
  n <- nrow(X)
  p <- ncol(X)
  X_ns = 
    do.call(
      cbind, 
      lapply(1:p, function(col){
        matrix(splines::ns(X[,col],df=7), n, 7)
      })
    )
  dim_ns = dim(X_ns)[2]
  X_ns = 
    stats::model.matrix(~.*.-1, data.frame(X_ns)) # pairwise interaction (not including squared term for each column)
  X_ns_sq = 
    do.call(cbind, lapply(1:dim_ns, function(col){matrix(X_ns[,col]^2)})) # squared term for each column
  X_ns = cbind(X_ns, X_ns_sq)
  return(X_ns)
}


slasso <- function(train_data,
                   pred_data = NULL){
  
  X <- train_data[,paste0("X.", 1:5)]
  W <- train_data$W
  U <- train_data$U
  Delta <- train_data$Delta
  
  X_ns <- get_ns_features(X = X)
  
  nobs <- nrow(X_ns); pobs = ncol(X_ns)
  k_folds = floor(max(3, min(10, nobs/4)))
  foldid = sample(rep(seq(k_folds), 
                      length = nobs))
  
  # Regress W + x_scl variable
  x_scl_tilde = cbind(1, W, X_ns)
  
  penalty_factor = c(0, rep(1, pobs + 1 ))
  
  s_fit = 
    glmnet::cv.glmnet(
      x_scl_tilde, 
      Surv(U, Delta), 
      family = "cox",
      foldid = foldid, 
      lambda = NULL,
      penalty.factor = penalty_factor,
      standardize = FALSE, 
      alpha = 1)
  
  s_beta = 
    as.vector(t(coef(s_fit, 
                     s = "lambda.min")[-1]))
  # obtain the coefficient on W
  tau_hat = s_beta[1]
  
  if (!is.null(pred_data)){
    # obtain the nuisance model for eta
    X_pred <- pred_data[,paste0("X.", 1:5)]
    X_ns_pred <- get_ns_features(X = X_pred)
    eta_0_hat <- X_ns_pred %*% s_beta[-1]
    
    eta_0_hat_vec <- eta_0_hat[,1]
  }else{
    eta_0_hat_vec <- NULL
  }
  
  return(list(tau_hat = tau_hat,
              eta_0_hat = eta_0_hat_vec))
}

fit_m_model <- function(X_ns, U, Delta, foldid) {
  m_fit <- glmnet::cv.glmnet(X_ns, Surv(U, Delta), family = "cox", foldid = foldid, standardize = FALSE)
  return(m_fit)
}

# Function to fit the p(X) model (Propensity score model)
fit_p_model <- function(X_ns, W, foldid) {
  p_fit <- glmnet::cv.glmnet(X_ns, W, family = "binomial", foldid = foldid, standardize = FALSE)
  return(p_fit)
}

cross_fit <- function(X_ns, W, U, Delta, k_folds) {
  nobs <- nrow(X_ns)
  foldid <- sample(rep(seq(k_folds), length = nobs))
  
  m_hat <- rep(NA, nobs)
  p_hat <- rep(NA, nobs)
  
  for (fold in seq(k_folds)) {
    # Train on the non-fold data
    train_idx <- which(foldid != fold)
    test_idx <- which(foldid == fold)
    
    # Subset foldid for the training set
    train_foldid <- foldid[train_idx]
    
    # Fit m(X) model on training data (subset foldid here)
    m_fit <- fit_m_model(X_ns[train_idx, ], U[train_idx], Delta[train_idx], train_foldid)
    m_hat[test_idx] <- predict(m_fit, newx = X_ns[test_idx, ], s = "lambda.min")
    
    # Fit p(X) model on training data (subset foldid here)
    p_fit <- fit_p_model(X_ns[train_idx, ], W[train_idx], train_foldid)
    p_hat[test_idx] <- predict(p_fit, newx = X_ns[test_idx, ], s = "lambda.min", type = "response")
  }
  
  return(list(m_hat = m_hat, p_hat = p_hat))
}

# Main R-Lasso function with cross-fitting
rlasso <- function(train_data, pred_data = NULL, k_folds = 5) {
  
  # Step 1: Extract Covariates, Treatment, Survival Time, and Censoring Indicator
  X <- as.matrix(train_data[,paste0("X.", 1:5)])  # Covariates as matrix
  W <- as.numeric(train_data$W)                  # Treatment as numeric vector (0 or 1)
  U <- as.numeric(train_data$U)                  # Survival time as numeric
  Delta <- as.numeric(train_data$Delta)          # Event indicator as numeric (1 or 0)
  
  # Apply the feature transformation using get_ns_features (same as in S-Lasso)
  X_ns <- get_ns_features(X = X)  # Transformed features
  
  nobs <- nrow(X_ns)
  
  # Perform cross-fitting to get m_hat (baseline survival) and p_hat (propensity score)
  cross_fit_results <- 
    cross_fit(X_ns, W, U, Delta, k_folds)
  m_hat <- cross_fit_results$m_hat
  p_hat <- cross_fit_results$p_hat
  
  # Compute residualized treatment (W - p(X))
  w_residual <- W - p_hat
  
  # Build the final design matrix
  x_tilde <- cbind(1, w_residual, X_ns)  # Adding transformed features from X_ns
  
  # Penalty factor: No penalty on the intercept, regular penalty on the residualized treatment and covariates
  penalty_factor <- c(0, 1, rep(1, ncol(X_ns)))  # No penalty on intercept, penalize treatment and covariates
  
  # Fit the final model for tau using W - p(X) and offset m(X)
  tau_fit <- glmnet::cv.glmnet(x_tilde,
                               Surv(U, Delta),
                               offset = m_hat,          # m(X) is set as the offset
                               family = "cox",
                               standardize = FALSE,
                               penalty.factor = penalty_factor)
  
  # Extract the coefficient for W - p(X), which represents the estimated treatment effect tau
  tau_beta <- as.vector(coef(tau_fit, s = "lambda.min"))
  tau_hat <- tau_beta[2]  # The coefficient for the residualized treatment w_residual
  
  # Prediction if pred_data is provided
  if (!is.null(pred_data)) {
    X_pred <- as.matrix(pred_data[, paste0("X.", 1:5)])
    X_ns_pred <- get_ns_features(X = X_pred)  # Transform the new data features
    eta_0_hat <- predict(tau_fit, newx = X_ns_pred, s = "lambda.min")
    eta_0_hat_vec <- eta_0_hat[, 1]
  } else {
    eta_0_hat_vec <- NULL
  }
  
  return(list(tau_hat = tau_hat, eta_0_hat = eta_0_hat_vec))
}