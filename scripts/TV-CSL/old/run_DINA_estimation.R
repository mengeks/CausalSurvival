run_DINA_estimation <- 
  function(single_data, 
           methods_DINA, 
           light_censoring, K = 2) {
    results <- list()
    if (methods_DINA$enabled) {
      # Create K-folds
      folds <- cut(seq(1, nrow(single_data)), breaks = K, labels = FALSE)
      
      # Loop over both nuisance methods and final model methods
      for (nuisance_method in methods_DINA$nuisance_method) {
        for (final_model_method in methods_DINA$final_model_method) {
          config_name <- paste(nuisance_method, final_model_method, sep = "_")
          
          start_time <- Sys.time()
          tau_estimates <- numeric(K)
          
          # Perform cross-fitting for K folds
          for (k in 1:K) {
            # Split the data: fold_k is the test set, all other folds are the training set
            fold_test <- single_data[folds == k, ]
            fold_train <- single_data[folds != k, ]
            
            # Estimate nuisance for fold_test using fold_train
            fold_test_nuisance <- DINA_estimate_nuisance(fold_train, fold_test, nuisance_method = nuisance_method, light_censoring = light_censoring)
            
            # Estimate tau for fold_test
            tau_estimates[k] <- fit_final_model(fold_test_nuisance, final_model_method)
          }
          
          # Average tau across K folds
          tau_est_DINA <- mean(tau_estimates)
          end_time <- Sys.time()
          
          # Calculate time taken
          time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
          
          # Store result for this configuration
          results[[config_name]] <- list(
            tau_estimate = tau_est_DINA,
            time_taken = time_taken
          )
        }
      }
    }
    return(results)
  }