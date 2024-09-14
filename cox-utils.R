library(survival)
source("R/datagen-helper.R")

give_p_value <- 
  function(R, 
           is_time_varying = FALSE, 
           is_linear = FALSE, 
           causal = FALSE, 
           path_for_sim_res,
           light_censoring = FALSE,
           outcome_type = "linear") {
  if (file.exists(path_for_sim_res)) {
    # saved_results<-readRDS("./outputs/time-varying-cox/causal_sim_non_linear_n_200_R_200.rds")
    saved_results <- readRDS(path_for_sim_res)
    ests_lambda_0 <- saved_results$ests_lambda_0
    ests_lambda_1 <- saved_results$ests_lambda_1
    rejects <- saved_results$rejects
    tau_ses <- saved_results$tau_ses
    tau_estimates <- saved_results$tau_estimates
    Zs <- saved_results$Zs
    start_r <- which.max((tau_estimates ==0)) + 1
    cat("Resuming from iteration:", start_r, "\n")
  } else {
    ests_lambda_0 <- ests_lambda_1 <- rejects <- tau_ses <- tau_estimates <- Zs <- numeric(R)
    start_r <- 1
  }
  
  for (r in start_r:R) {
    # r <- 2
    print(r)
    set.seed(r * 100 + 23)
    simulated_data <- generate_simulated_data(
      is_time_varying = is_time_varying, 
      is_linear = is_linear,
      light_censoring = light_censoring
    )
    
    if (is_time_varying) {
      model <- 
        time_varying_estimate(
          simulated_data = simulated_data, 
          causal = causal
        )
    } else {
      model <- 
        non_time_varying_estimate(
          simulated_data = simulated_data, 
          causal = causal,
          light_censoring = light_censoring,
          outcome_type = outcome_type)
    }
    
    model_summary <- summary(model)
    tau_estimates[r] <- tau_estimate <- model_summary$coefficients[1, 1]
    tau_ses[r] <- tau_se <- model_summary$coefficients[1, 3]
    Zs[r] <- Z <- (tau_estimate - tau) / tau_se
    rejects[r] <- abs(Z) > 1.96
    print(paste0("tau_estimate: ",tau_estimate))
    print(paste0("tau_se: ",tau_se))
    print(paste0("Reject is ", abs(Z) > 1.96))
    # Save intermediate results every 20 iterations
    if (r %% 20 == 0 || r == R) {
      saveRDS(list(
        ests_lambda_0 = ests_lambda_0,
        ests_lambda_1 = ests_lambda_1,
        tau_estimates = tau_estimates,
        tau_ses = tau_ses,
        Zs = Zs,
        rejects = rejects
      ), file = path_for_sim_res)
      cat("Saved intermediate results at iteration:", r, "\n")
    }
  }
  
  return(list(
    tau_estimates = tau_estimates,
    tau_ses = tau_ses,
    Zs = Zs,
    rejects = rejects
  ))
}


