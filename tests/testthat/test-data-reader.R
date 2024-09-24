library(testthat)
source("R/data-reader.R")

test_that("read_single_simulation_data works correctly for time-varying data", {
  # Read a single simulated dataset
  single_data <- read_single_simulation_data(
    n = 500, R = 200, is_time_varying = TRUE, i = 1, 
    eta_type = "linear-interaction", 
    baseline_type = "cosine", 
    folder_name = "data/simulated"
  )
  
  # Extract the data and parameters
  tmp <- single_data$data
  tmp_params <- single_data$params
  
  # Ensure the dataset is not NULL
  expect_true(!is.null(tmp), info = "Data should not be NULL")
  
  # Ensure the parameters are not NULL
  expect_true(!is.null(tmp_params), info = "Params should not be NULL")
  
  
  
  # Ensure parameters contain specific expected elements
  expect_true("tau" %in% names(tmp_params), info = "Params should contain 'tau'")
  expect_true("light_censoring" %in% names(tmp_params), info = "Params should contain 'light_censoring'")
  
  # Ensure tau is a numeric value
  expect_true(is.numeric(tmp_params$tau), info = "'tau' should be numeric")
})

