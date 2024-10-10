read_single_simulation_data <- 
  function(n, is_time_varying=NULL, 
           i = NULL, 
           eta_type = "linear-interaction", 
           baseline_type = "linear",
           CATE_type = "constant",
           folder_name = "data") {
  
  # subfolder <- if (is.null(is_time_varying)){
  #   ""
  # }
  #   else if (is_time_varying) {
  #   "time-varying"
  # } else {
  #   "non-time-varying"
  # }
  # 
  path_for_sim_data <- here::here(
    folder_name, 
    paste0(eta_type, "_", CATE_type)
  )
  
  
  file_name <- paste0("sim_data_", i, 
                      "_n_", n,
                      ".rds")
  
  file_path <- file.path(path_for_sim_data, file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  
  data <- readRDS(file_path)
  
  cat("Loaded dataset", i, "from", file_path, "\n")
  
  return(data)
}



read_TV_CSL_nuisance_data <- function(k = 1, data_path = here("scripts/TV-CSL/tests/data")) {
  file_path <- file.path(data_path, paste0("TV_CSL_nuisance_data_k", k, ".RData"))
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  load(file_path)
  
  list(
    fold_nuisance = fold_nuisance,
    fold_causal = fold_causal,
    train_data_original_nuisance = train_data_original_nuisance
  )
}

