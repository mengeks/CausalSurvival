library(testthat)
source("R/TV-CSL-result-process.R")
test_that("generate_directory_path generates the correct path", {
  # Create a mock config list
  config <- list(
    experiment = list(n = 100, R = 10, eta_type = "linear", baseline_type = "constant")
  )
  
  # Test the function
  base_dir <- "data/outputs/TV-CSL"
  generated_path <- 
    generate_directory_path(base_dir, config)
  
  print(generated_path)
  
  # Check if the generated path is correct
  expected_path <- "data/outputs/TV-CSL/linear_constant/n_100/R_10"
  expect_equal(generated_path, expected_path)
})

test_that("generate_output_dir works for a mock json", {
  # Create a mock config list
  config <- list(
    n = 100, R = 10, eta_type = "linear", baseline_type = "constant"
  )
  
  # Test the function
  generated_output_dir <- 
    generate_output_dir(config)
  
  # Check if the generated output directory path is correct
  expected_output_dir <- 
    "data/outputs/TV-CSL/linear_constant/n_100"
  expect_equal(generated_output_dir, expected_output_dir)
})


test_that("generate_output_dir works for a real JSON",{
  json_file <- "scripts/TV-CSL/linear-interaction_cosine/config-n-200.json"
  config <- load_experiment_config(json_file)
  output_dir <- generate_output_dir(config)
  expected_output_dir <- 
    "data/outputs/TV-CSL/linear-interaction_cosine/n_200"
  expect_equal(output_dir, expected_output_dir)
})

test_that("read_results reads results correctly from mock files", {
  # Create a temporary directory and mock results
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  
  # Create a mock result file
  R_value <- 200
  mock_results <- list(a = 1, b = 2)
  saveRDS(mock_results, file = file.path(temp_dir, paste0("R_", R_value, "_results.rds")))
  
  # Test the function with the replication number
  results <- read_results(temp_dir, R = R_value)
  
  # Check if the results are read correctly
  expect_equal(results$a, 1)
  expect_equal(results$b, 2)
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
})



test_that("read_results reads results correctly from true files", {
  output_dir <- "data/outputs/TV-CSL/linear-interaction_cosine/n_200"
  
  # Ensure the file exists before attempting to read it
  results_file <- file.path(output_dir, "R_200_results.rds")
  
  if (file.exists(results_file)) {
    results <- read_results(output_dir, R = 200)
    
    # Assuming the structure of your results is known, you can write specific tests.
    # For example:
    expect_true(is.list(results))
    expect_named(results)  # Check if results has named elements
    # Add more checks based on the structure of the results
  } else {
    skip(paste("The file", results_file, "does not exist, skipping test."))
  }
})


test_that("prepare_metrics_dataframe works as expected", {
  # Mock input data for Cox, S-Lasso, and DINA
  cox_metrics_list <- list(
    `correctly-specified_TRUE` = c(bias = 0.01, se = 0.2, mse = 0.04),
    `correctly-specified_FALSE` = c(bias = -2.01, se = 0.21, mse = 4.1)
  )
  
  slasso_metrics <- 
    c(bias = -1.5, se = 0.5, mse = 2.0)
  
  DINA_metrics <- list(
    `config_A` = c(bias = 0.005, se = 0.15, mse = 0.03),
    `config_B` = c(bias = -0.005, se = 0.17, mse = 0.035)
  )
  
  # Run the function
  metrics_df <- 
    prepare_metrics_dataframe(
      cox_metrics_list, 
      slasso_metrics, 
      DINA_metrics)
  
  # Expected dataframe check
  expect_true(ncol(metrics_df) == 5)
  expect_true(all(c("Method", "Specification", "Bias", "SE", "MSE") %in% colnames(metrics_df)))
  expect_equal(metrics_df$Method, c("Cox", "Cox", "S-Lasso", "DINA", "DINA"))
  expect_equal(metrics_df$Bias[1], 0.01)
  expect_equal(metrics_df$Bias[4], 0.005)
})

