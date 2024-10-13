generate_path <- 
  function(is_linear, 
           causal,
           n, 
           R, 
           data_folder,
           light_censoring=F,
           outcome_type = "linear") {
    path <- paste0(data_folder,
                   ifelse(causal,"causal_",""),
                   ifelse(outcome_type=="linear","lin_est_","non_lin_est_"),
                   ifelse(light_censoring,"light_censoring_",""),
                   ifelse(is_linear,"sim_linear_n_", "sim_non_linear_n_"),
                   n,"_R_",R,".rds")
    return(path)
  }

report_results <- function(
    is_linear,causal, n, R, 
    data_folder, plot_folder,
    light_censoring=F,
    outcome_type = "linear") {
  # Generate the path for simulation results
  path_for_sim_res <- 
    generate_path(
      is_linear, causal, n, R, 
      data_folder,light_censoring,outcome_type)
  
  # Read the simulation results
  sim_res <- readRDS(file = path_for_sim_res)
  
  # Calculate and print the estimated p-value
  mean_est <- mean(sim_res$tau_estimates)
  cat("The mean estimated tau is:", mean_est, "\n")
  
  cat("The sd of estimated tau is:", sd(sim_res$tau_estimates), "\n")
  
  # Calculate and print the estimated p-value
  p_val_RCT <- 1 - mean(sim_res$rejects)
  cat("The estimated p-value is:", p_val_RCT, "\n")
  
  hist(sim_res$tau_estimates, 
       main = "Histogram of Estimated Tau", 
       xlab = "Estimated Tau", 
       col = "lightblue",
       xlim = c(0,2),
       breaks = 10 )
  abline(v = tau, col = "red")
  
  # Save the plot
  linearity_label <- ifelse(is_linear, "_linear", "_non_linear")
  causal_label <- ifelse(causal, "_causal", "")
  light_censoring_label <- ifelse(light_censoring,"_light_censoring", "")
  outcome_type_label <- ifelse(outcome_type=="linear","_lin_est","_non_lin_est")
  plot_file_path <- 
    paste0(plot_folder, 
           "tau_estimates_histogram", 
           linearity_label,
           causal_label, 
           light_censoring_label,
           ".png")
  
  dev.copy(png, filename = plot_file_path)
  dev.off()
  
  cat("Histogram saved to:", plot_file_path, "\n")
}
