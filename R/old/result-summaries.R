calculate_bias_se_mse <- 
  function(tau_estimates, tau_true) {
    tau_diff <- tau_estimates - tau_true
    bias <- mean(tau_diff)
    se <- sd(tau_diff)
    mse <- bias^2 + se^2
    return(c(bias = bias, se = se, mse = mse))
  }


