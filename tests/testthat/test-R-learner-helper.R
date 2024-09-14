source("R/R-learner-helper.R")
library(ggplot2)

# Test generate_eta_0 function with a controlled input (only X.1 has non-zero values)
test_generate_eta_0 <- function() {
  p <- 5  # Number of predictors
  
  # Generate X with only X.1 non-zero (sequence from -1 to 1), other columns zero
  X <- matrix(0, 10, p)
  X[,1] <- seq(-1, 1, length.out = 10)  # X.1 is a sequence from -1 to 1, others are 0
  
  # Generate eta_0 for both outcome types
  eta_linear <- generate_eta_0(X, "linear-interaction")
  eta_log <- generate_eta_0(X, "log")
  
  # Print the values of eta_0
  print("Testing generate_eta_0 with 'linear-interaction'")
  print(eta_linear)
  
  print("Testing generate_eta_0 with 'log'")
  print(eta_log)
  
  # Plot eta_0 for 'linear-interaction'
  df_linear <- data.frame(X1 = X[,1], eta_0 = eta_linear)
  p_linear <- ggplot(df_linear, aes(x = X1, y = eta_0)) +
    geom_line() +
    ggtitle("eta_0 (Linear Interaction)") +
    labs(x = "X.1", y = "eta_0") +
    theme_minimal()
  
  # Plot eta_0 for 'log'
  df_log <- data.frame(X1 = X[,1], eta_0 = eta_log)
  p_log <- ggplot(df_log, aes(x = X1, y = eta_0)) +
    geom_line() +
    ggtitle("eta_0 (Log)") +
    labs(x = "X.1", y = "eta_0") +
    theme_minimal()
  
  # Print the plots
  print(p_linear)
  print(p_log)
}

# Run the test
test_generate_eta_0()


# Test run_simulation function to verify that files are saved correctly
test_run_simulation <- function() {
  # Set small parameters for the test
  n <- 100
  R <- 10
  outcome <- "linear-interaction"
  
  # Run simulation and time it
  start_time <- Sys.time()
  run_simulation(n = n, R = R, outcome = outcome)
  end_time <- Sys.time()
  
  time_taken <- end_time - start_time
  cat("Time taken for run_simulation:", time_taken, "seconds\n")
  
  # Check if the file is saved correctly
  output_file <- here::here(
    "data/outputs/continuous-R-learner",
    paste0("n_", n, "_R_", R, "_outcome_", outcome, ".rds")
  )
  
  if (file.exists(output_file)) {
    cat("File saved successfully at", output_file, "\n")
  } else {
    stop("File was not saved!")
  }
}

test_run_simulation()

