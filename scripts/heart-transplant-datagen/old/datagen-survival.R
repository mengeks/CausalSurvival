
source("R/datagen-helper.R")

n_list <- c(200, 500, 1000)
R <- 200
HTE_type_list <- c("zero", "constant", "ReLU", "linear", "non-linear")
eta_type_list <- c("linear", "non-linear","10-dim-non-linear")

other_params <- list(
  is_time_varying = T,
  light_censoring = F,
  lambda_C = 0.1,
  p = 10,
  X_distribution = "normal", 
  X_cov_type = "toeplitz",
  tx_difficulty = "simple"
)
verbose = 0
cores = detectCores() - 1

for (n in n_list) {
  for (eta_type in eta_type_list){
    for (HTE_type in HTE_type_list) {
      
      if (verbose==2){
        print(paste("n:", n,"eta_type:", eta_type,"HTE_type:",HTE_type, sep="   "))
      }
      
      params <- 
        params <- c(list(
          n = n,
          HTE_type = HTE_type,
          eta_type = eta_type
        ),other_params)
      
      folder_name <- if (params$is_time_varying) "data/simulated" else "data/simulated/non-time-varying"
      path_for_sim_data <- here::here(
        folder_name, paste0(eta_type, "_", HTE_type)
      )
      
      dir.create(path_for_sim_data, 
                 showWarnings = FALSE, 
                 recursive = TRUE)
      if (verbose == 1){
        print( "Save path:" )
        print(path_for_sim_data)
      }
      for (group_start in seq(1, R, by = 20)) {
        group_end <- min(group_start + 19, R)
        time_taken <- system.time({
          mclapply(group_start:group_end, 
                   generate_and_save_data, 
                   n = n, 
                   is_time_varying = is_time_varying, 
                   path_for_sim_data = path_for_sim_data, 
                   params = params,  # Pass params with eta_type and baseline_type included
                   mc.cores = cores)
        })
        print(paste("Time taken for iterations", group_start, "to", group_end, ":", time_taken[3], "seconds"))
      }
    } # end for HTE_type_list
  } # end for eta_type_list
} # end for n_list



# Use verbose to print
# verbose = 1: progress bar / which iteration / which functions
#     also output times it uses 
# verbose = 2: verbose = 1 + exact things that are passed through  
