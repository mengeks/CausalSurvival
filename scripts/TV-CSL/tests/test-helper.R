load_or_generate_test_data_m_regression <- function(
    n = 2000,
    lambda_C = 0.1,
    eta_type = "10-dim-linear",
    CATE_type = "zero",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 42
) {
  source("R/datagen-helper.R")
  
  file_path <- here::here(
    "scripts", "TV-CSL", "tests", "data",
    sprintf("eta-type-%s_CATE-type-%s_n-%d.rds", eta_type, CATE_type, n)
  )
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(file_path)) {
    train_data <- generate_simulated_data(
      n = n,
      lambda_C = lambda_C,
      eta_type = eta_type,
      CATE_type = CATE_type,
      seed_value = seed_value,
      linear_intercept = intercept,
      linear_slope_multiplier = slope_multiplier,
      verbose = 0
    )
    
    train_data_pseudo <- preprocess_data(
      single_data = train_data,
      run_time_varying = TRUE
    )
    
    saveRDS(
      list(
        train_data = train_data,
        train_data_pseudo = train_data_pseudo,
        parameters = list(
          n = n,
          lambda_C = lambda_C,
          eta_type = eta_type,
          CATE_type = CATE_type,
          intercept = intercept,
          slope_multiplier = slope_multiplier,
          seed_value = seed_value
        )
      ),
      file = file_path
    )
  }
  
  loaded_data <- readRDS(file_path)
  return(loaded_data)
}


calculate_min_relative_error <- function(true_coef, est_coef) {
  if (length(true_coef) != length(est_coef)) {
    stop("True and estimated coefficients must have the same length")
  }
  
  diff <- true_coef - as.vector(est_coef)
  relative_diff <- abs(diff / true_coef)
  min_idx <- which.min(relative_diff)
  
  list(
    min_relative_error = relative_diff[min_idx],
    min_index = min_idx,
    true_value = true_coef[min_idx],
    estimated_value = est_coef[min_idx]
  )
}