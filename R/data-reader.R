read_single_simulation_data <- 
  function(n, R, is_time_varying, i, 
           eta_type = "linear-interaction", 
           baseline_type = "cosine", 
           folder_name = "data/simulated") {
  
  subfolder <- if (is_time_varying) {
    "time-varying"
  } else {
    "non-time-varying"
  }
  
  
  path_for_sim_data <- here::here(
    folder_name, subfolder, paste0(eta_type, "_", baseline_type), paste0("sim_data_n_", n, "_R_", R)
  )
  
  
  seed_value <- 123 + 11 * i
  file_name <- paste0("sim_data_", i, "_seed_", seed_value, ".rds")
  
  # Full path to the specific file
  file_path <- file.path(path_for_sim_data, file_name)
  
  # Check if the file exists before trying to read it
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  
  # Read the .rds file
  data <- readRDS(file_path)
  
  cat("Loaded dataset", i, "from", file_path, "\n")
  
  return(data)
}
