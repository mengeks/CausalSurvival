library(tidyverse)
library(survival)
library(here)
source("R/datagen-helper.R")

n_list <- c(200, 500, 1000)
R <- 200
CATE_type_list <- c("zero", "linear", "non-linear")
eta_type_list <- c("10-dim-linear", "non-linear","10-dim-non-linear")

other_params <- list(
  light_censoring = F,
  lambda_C = 0.1,   
  p = 10,
  X_distribution = "normal", 
  X_cov_type = "toeplitz",
  tx_difficulty = "simple"
)

params <- c(list(
  n = n,
  CATE_type = CATE_type,
  eta_type = eta_type
),other_params)



run_datagen(
  n_list = n_list, 
  R = R, 
  is_time_varying_range = T, 
  CATE_type_list = CATE_type_list,
  eta_type_list = eta_type_list,
  other_params = other_params, 
  verbose = 0
)

# Use verbose to print
# verbose = 1: progress bar / which iteration / which functions
#     also output times it uses 
# verbose = 2: verbose = 1 + exact things that are passed through  
