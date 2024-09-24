source("R/time-varying-estimate.R")
library(testthat)

library(testthat)

test_that("generate_regressor_part returns correct formula components", {
  expect_equal(generate_regressor_part(model_spec = "correctly-specified"),
               "W + X.1 + X.2 + X.3 + X.4 + X.5 + X.1:X.2")
  expect_equal(generate_regressor_part(model_spec = "mildly-mis-specified"),
               "W + X.1 + X.2 + X.3 + X.4 + X.5")
  expect_equal(generate_regressor_part(model_spec = "quite-mis-specified"),
               "W + X.1 + X.2")
})

test_that("create_cox_formula creates correct formulas", {
  formula_correct <- 
    create_cox_formula(
      model_spec = "correctly-specified", 
      run_time_varying = TRUE)
  expect_equal(as.character(formula_correct),
          c( "~","Surv(tstart, tstop, Delta)", "W + X.1 + X.2 + X.3 + X.4 + X.5 + X.1:X.2"))
  
  formula_mis <- create_cox_formula(model_spec = "mildly-mis-specified", run_time_varying = FALSE)
  expect_equal(as.character(formula_mis),
               c("~", "Surv(U, Delta)", "W + X.1 + X.2 + X.3 + X.4 + X.5"))
})

test_that("preprocess_data returns appropriate dataset", {
  single_data <- data.frame(A = c(1, 2), U = c(3, 4), X.1 = c(0.5, 0.8), Delta = c(1, 0))
  
  result_static <- 
    preprocess_data(single_data = single_data, 
                    run_time_varying = FALSE)
  expect_true("W" %in% colnames(result_static))
})

test_that("create_pseudo_dataset generates correct pseudo dataset", {
  # Test survival data
  survival_data <- data.frame(
    U = c(5, 3),
    Delta = c(1, 0),
    A = c(2, 4),
    id = 1:2,
    X1 = c(1, 2),
    X2 = c(0.5, 1.5)
  )
  
  pseudo_dataset <- create_pseudo_dataset(survival_data)
  
  # Check if pseudo_dataset has the correct number of rows
  expect_equal(nrow(pseudo_dataset), 3)  # 2 rows for the first subject, 1 for the second
  
  # Check if the tstart, tstop, Delta, and W columns are correct
  expect_equal(pseudo_dataset$tstart, c(0, 2, 0))
  expect_equal(pseudo_dataset$tstop, c(2, 5, 3))
  expect_equal(pseudo_dataset$Delta, c(0, 1, 0))
  expect_equal(pseudo_dataset$W, c(0, 1, 0))
  
  # Check if the covariates are retained in the pseudo dataset
  expect_equal(pseudo_dataset$X1, c(1, 1, 2))
  expect_equal(pseudo_dataset$X2, c(0.5, 0.5, 1.5))
  expect_equal(pseudo_dataset$id, c(1, 1, 2))
  
  # Check if time difference (tstop - tstart) > 0.001
  expect_true(all((pseudo_dataset$tstop - pseudo_dataset$tstart) > 0.001))
})

test_that("cox_model_estimation returns valid tau estimate", {
  loaded_data <- read_single_simulation_data(
    n = 500, R = 200, is_time_varying = TRUE, i = 1, 
    eta_type = "linear-interaction", 
    baseline_type = "cosine", 
    folder_name = "data/simulated"
  )
  
  single_data <- loaded_data$data
  
  methods_cox <- 
    list(model_spec = "correctly-specified", 
         run_time_varying = T)
    
  # We expect the actual model to run, assuming the survival data is complete
  result <- cox_model_estimation(
    single_data = single_data, 
    methods_cox = methods_cox,
    light_censoring = NULL)
  
  expect_true(!is.na(result))  # Assuming model fitting works
  
  
  methods_cox <- 
    list(model_spec = "correctly-specified", 
         run_time_varying = F)
  
  # We expect the actual model to run, assuming the survival data is complete
  result <- cox_model_estimation(
    single_data = single_data, 
    methods_cox = methods_cox,
    light_censoring = NULL)
  
  expect_true(!is.na(result))  # Assuming model fitting works
})
