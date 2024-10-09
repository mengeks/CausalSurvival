## Test Lasso
source("R/data-reader.R")
source("scripts/TV-CSL/time-varying-estimate.R")
n = 500; i <- 1
eta_type <- "10-dim-non-linear"
CATE_type <- "linear"
train_data_origin <- 
  read_single_simulation_data(
    n = n, 
    i = i, 
    eta_type = eta_type,
    CATE_type = CATE_type)$data

train_data <- 
  preprocess_data(single_data = train_data_origin, 
                  run_time_varying = T)
test_data <- 
  read_single_simulation_data(
    n = n, 
    i = i + 100, 
    eta_type = eta_type,
    CATE_type = CATE_type)$data


regressor_spec <- "linear-only"
CATE_spec <- "correctly-specified"
source("scripts/TV-CSL/time-varying-estimate.R")
## Test S_lasso <- function(train_data, test_data, regressor_spec, CATE_spec)
lasso_ret <-
  S_lasso(train_data = train_data,
          test_data = test_data,
          regressor_spec = regressor_spec,
          CATE_spec = CATE_spec)


