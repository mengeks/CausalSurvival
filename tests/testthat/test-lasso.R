source("R/lasso.R")
dataset_file <-
  here::here(
    "data/simulated/non-time-varying/sim_data_n_500_R_200/sim_data_1_seed_134.rds")
single_data <- readRDS(dataset_file)$data  # Adjust the path as needed
tau_est_slasso <- slasso(single_data)
tau_est_slasso$tau_hat

source("R/lasso.R")
tau_est_rlasso <- rlasso(single_data)
tau_est_rlasso$tau_hat
