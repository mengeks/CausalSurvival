library(tidyverse)
library(survival)
library(here)
source("R/datagen-helper.R")

# Set simulation parameters
params <- list(
  light_censoring = TRUE,  # Assuming light_censoring is set to TRUE in your runs
  lambda_C = 20,           # Based on light_censoring = TRUE
  tau = 1,
  p = 5,
  beta = rep(1, 5),        # 5 predictors, all with coefficient 1
  delta = 0.5,
  # eta_type = "linear-interaction", 
  eta_type = "log",
  # baseline_type = "cosine"
  baseline_type = "cosine"
)

n_list <- c(200, 500, 1000)
R <- 200
# is_time_varying_range <- c(TRUE, FALSE)
is_time_varying_range <- c(FALSE)
source("R/datagen-helper.R")
run_simulation(
  n_list = n_list, 
  R = R, 
  is_time_varying_range = is_time_varying_range, 
  params = params
)
