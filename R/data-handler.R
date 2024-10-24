read_single_simulation_data <- 
  function(n, is_time_varying=NULL, 
           i = NULL, 
           eta_type = "linear-interaction", 
           baseline_type = "linear",
           CATE_type = "constant",
           folder_name = "data") {
  
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



RESULTS_DIR <- "scripts/TV-CSL/results/"

#' Generate file paths for experiment results and save data to CSV
#'
#' @param is_running_cox Logical, whether cox method is running
#' @param is_running_lasso Logical, whether lasso method is running
#' @param is_running_TV_CSL Logical, whether TV-CSL method is running
#' @param eta_type Character, the eta type
#' @param CATE_type Character, the CATE type
#' @param n Integer, the sample size
#' @param i Integer, the iteration number
#' @param seed_value Integer, the seed value for reproducibility
#' 
#' @return The full path of the saved CSV file
generate_output_path <- function(results_dir = "scripts/TV-CSL/results/",
                                 is_running_cox, 
                                 is_running_lasso, 
                                 is_running_TV_CSL, 
                                 eta_type, 
                                 CATE_type, 
                                 n, 
                                 i, 
                                 seed_value) {
  
  method_setting <- paste0(
    ifelse(is_running_cox, "cox_", ""),
    ifelse(is_running_lasso, "lasso_", ""),
    ifelse(is_running_TV_CSL, "TV-CSL_", "")
  )
  
  dgp_setting <- paste0("eta-", eta_type, "_CATE-", CATE_type)
  
  
  output_folder <- paste0(
    results_dir,
    method_setting, 
    dgp_setting, "_n-", n, "/"
  )
  
  result_csv_file <- paste0(
    output_folder, 
    "result-iteration_", i, "-seed_", seed_value, ".csv"
  )
  
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  
  return(result_csv_file)
}


