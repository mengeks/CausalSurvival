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
  # train_data <- pred_data <- simulated_data
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
