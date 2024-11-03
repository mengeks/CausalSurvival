

### These are not tests. These are code that that 

source("R/data-handler.R")
n = 500; i <- 1
eta_type <- "non-linear"
HTE_type <- "linear"
train_data_origin <- 
  read_single_simulation_data(
    n = n, 
    i = i, 
    eta_type = eta_type,
    HTE_type = HTE_type)$data

train_data <- 
  preprocess_data(single_data = train_data_origin, 
                  run_time_varying = T)
test_data <- 
  read_single_simulation_data(
    n = n, 
    i = i + 100, 
    eta_type = eta_type,
    HTE_type = HTE_type)$data


# regressor_spec <- "linear"
regressor_spec <- "complex"
HTE_spec <- "linear"
source("scripts/TV-CSL/time-varying-estimate.R")

## Test S_lasso <- function(train_data, test_data, regressor_spec, HTE_spec)
lasso_ret <-
  S_lasso(train_data = train_data,
          test_data = test_data,
          regressor_spec = regressor_spec,
          HTE_spec = HTE_spec)
lasso_ret$beta_eta_0
lasso_ret$beta_HTE

