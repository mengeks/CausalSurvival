run_result <- function(
    is_linear, 
    is_time_varying, 
    causal = FALSE,
    light_censoring = F,
    outcome_type = "linear") {
  folder_to_save <- ifelse(
    is_time_varying,
    "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/time-varying-cox/",
    "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/non-time-varying-cox/"
  )
  
  path_for_sim_res <- generate_path(
    is_linear = is_linear,
    causal = causal,
    n = n,
    R = R,
    data_folder = folder_to_save,
    light_censoring = light_censoring,
    outcome_type = outcome_type
  )
  
  print(path_for_sim_res)
  tictoc::tic()
  
  sim_res <- give_p_value(
    R = R,
    is_time_varying = is_time_varying,
    is_linear = is_linear,
    causal = causal,
    path_for_sim_res = path_for_sim_res,
    light_censoring = light_censoring,
    outcome_type = outcome_type
  )
  
  tictoc::toc()
  
  saveRDS(sim_res, file = path_for_sim_res)
}
generate_path <- 
  function(is_linear, 
           causal,
           n, 
           R, 
           data_folder,
           light_censoring=F,
           outcome_type = "linear") {
    path <- paste0(data_folder,
                   ifelse(causal,"causal_",""),
                   ifelse(outcome_type=="linear","lin_est_","non_lin_est_"),
                   ifelse(light_censoring,"light_censoring_",""),
                   ifelse(is_linear,"sim_linear_n_", "sim_non_linear_n_"),
                   n,"_R_",R,".rds")
    return(path)
  }