library(rlearner)
expit <- function(x) { 1 / (1 + exp(-x)) }

# Function to generate eta_0 based on outcome type
generate_eta_0 <- function(X, outcome) {
  if (outcome == "linear-interaction") {
    gamma <- c(1, 1, 1, 1, 1)  # Linear coefficients
    delta <- 0.5               # Interaction coefficient
    return(X %*% gamma + delta * X[,1] * X[,2])
  } else if (outcome == "log") {
    return(2 * log(1 + exp(X[,1] + X[,2] + X[,3])))
  } else {
    stop("Invalid outcome type")
  }
}

run_simulation <- function(n, R, outcome) {
  tau_est_rlasso <- tau_est_slasso <- numeric(R)
  time_rlasso <- time_slasso <- numeric(R)  # Store time for each method
  
  for (i in 1:R) {
    print(paste("Running iteration", i, "for n =", n, "and outcome =", outcome))
    
    p <- 5
    X <- matrix(runif(n * p, min = -1, max = 1), n, p)
    tau <- 1
    eta_0 <- generate_eta_0(X, outcome)
    
    e <- expit(X[,2] + X[,3])
    W <- rbinom(n, 1, e)
    epsilon <- rnorm(n, 0, 1)
    Y <- eta_0 + W * tau + epsilon
    
    # Time rlasso
    start_rlasso <- Sys.time()
    rlasso_fit <- rlasso(x = X, w = W, y = Y)
    rlasso_est <- predict(rlasso_fit, x = X)
    time_rlasso[i] <- Sys.time() - start_rlasso
    tau_est_rlasso[i] <- rlasso_est[1,1]
    
    # Time slasso
    start_slasso <- Sys.time()
    slasso_fit <- slasso(x = X, w = W, y = Y)
    slasso_est <- predict(slasso_fit, x = X)
    time_slasso[i] <- Sys.time() - start_slasso
    tau_est_slasso[i] <- slasso_est[1,1]
  }
  
  results <- data.frame(
    tau_est_rlasso = tau_est_rlasso, 
    tau_est_slasso = tau_est_slasso,
    time_rlasso = time_rlasso,
    time_slasso = time_slasso
  )
  
  output_file <- here::here(
    "data/outputs/continuous-R-learner",
    paste0("n_", n, "_R_", R, "_outcome_", outcome, ".rds")
  )
  
  saveRDS(results, file = output_file)
  cat("Results saved to", output_file, "\n")
}
