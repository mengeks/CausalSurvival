library(testthat)
source("R/data-handler.R")

test_that("Test generate_output_path function", {
  
  json_file <- "scripts/TV-CSL/params.json"
  config <- load_experiment_config(json_file)
  
  n <- config$n
  methods <- config$methods
  eta_type <- config$eta_type
  CATE_type <- config$CATE_type
  R <- config$R
  

  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  
  # Iteration and seed values
  i <- 1
  seed_value <- 123 + 11 * i
  
  result_csv_file <- generate_output_path(
    is_running_cox = is_running_cox,
    is_running_lasso = is_running_lasso,
    is_running_TV_CSL = is_running_TV_CSL,
    eta_type = eta_type,
    CATE_type = CATE_type,
    n = n,
    i = i,
    seed_value = seed_value
  )
  
  expected_folder <- paste0("scripts/TV-CSL/results/",
                            ifelse(is_running_cox, "cox_", ""),
                            ifelse(is_running_lasso, "lasso_", ""),
                            ifelse(is_running_TV_CSL, "TV-CSL_", ""),
                            "eta-", eta_type, "_CATE-", CATE_type, "_n-", n, "/")
  
  expected_file <- paste0(expected_folder, "result-iteration_", i, "-seed_", seed_value, ".csv")
  
  
  expect_equal(result_csv_file, expected_file)
  
  dir.create(expected_folder, showWarnings = FALSE, recursive = TRUE)
  expect_true(dir.exists(expected_folder))
})



test_that("read_single_simulation_data works correctly for new data", {
  single_data <- read_single_simulation_data(
    n = 500, 
    R = 200, 
    i = 1, 
    eta_type = "10-dim-non-linear",
    CATE_type = "linear"
  )
  
  tmp <- single_data$data
  tmp_params <- single_data$params
  
  expect_true(!is.null(tmp), info = "Data should not be NULL")
  
  expect_true(!is.null(tmp_params), info = "Params should not be NULL")
  
  # expect_true("tau" %in% names(tmp_params), info = "Params should contain 'tau'")
  expect_true("light_censoring" %in% names(tmp_params), info = "Params should contain 'light_censoring'")
  
  # expect_true(is.numeric(tmp_params$tau), info = "'tau' should be numeric")
})



test_that("read_single_simulation_data works correctly for old data", {
  single_data <- read_single_simulation_data(
    n = 500, 
    R = 200, 
    is_time_varying = TRUE,
    i = 1, 
    eta_type = "linear-interaction", 
    baseline_type = "cosine", 
    folder_name = "data/old/simulated"
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

