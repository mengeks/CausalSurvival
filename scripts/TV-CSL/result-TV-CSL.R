library(jsonlite)
library(here)
library(dplyr)
source("scripts/TV-CSL/result-process.R")
source("scripts/TV-CSL/time-varying-estimate.R")

eta_type <- "non-linear"; HTE_type <- "linear"


## Gather table for TV-CSL
source("scripts/TV-CSL/result-process.R")
json_file_TV_CSL <- "scripts/TV-CSL/params-TV-CSL.json"
results <- evaluate_HTE_metrics(
  json_file = json_file_TV_CSL,
  results_dir = "scripts/TV-CSL/results/",
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
  # n_list = 200,
  HTE_spec = "linear"
)



json_file_lasso <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  json_file = json_file_lasso,
  eta_type = eta_type,
  HTE_type = HTE_type,
)



process_results_to_csv(
  json_file =  json_file_TV_CSL,
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
)

two ways of specifying method
A: input the json file, or the method variable of the json file; 
  pro: this is one variable; you let the functions to 
  con;
B: manually turn on / of f
  pro: 
  con: you have to turn two variables (turn off lasso and turn on) at once 


input: 
  json_file =  json_file_TV_CSL, # json_file of the current run or the past run
  results_dir = "scripts/TV-CSL/results/", # where to seach for 
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
  HTE_spec = HTE_spec

Steps:
  config <- load_experiment_config(json_file)

running_flags <- get_is_running_flags(methods)

is_running_cox <- running_flags$is_running_cox
is_running_lasso <- running_flags$is_running_lasso
is_running_TV_CSL <- running_flags$is_running_TV_CSL

aggregated_metrics <- NULL
  for (n in n_list){
    # 1. go to the right folder 
    method_setting <- paste0(
      ifelse(is_running_cox, "cox_", ""),
      ifelse(is_running_lasso, "lasso_", ""),
      ifelse(is_running_TV_CSL, "TV-CSL_", "")
    )
    
    output_folder <- generate_output_folder(
      results_dir = results_dir,
      method_setting = method_setting, 
      eta_type = eta_type, 
      HTE_type = HTE_type, 
      n = n
    )
    
    HTE_coef_file <- paste0("HTE-spec-", HTE_spec, "_beta-HTE.csv")
    
    result_csv_file <- paste0(
      output_folder, 
      HTE_coef_file
    )
    
    if (file.exists(result_csv_file)) {
      df_HTE_coef <- read.csv(result_csv_file)
    } else {
      cat("Warning: File not found for ...
    }
    
    
    ## Evaluate the coefficients 
    read one test set
    #'   test_data <- 
#'     read_single_simulation_data(
#'       n = 2000, 
#'       i = 101, 
#'       eta_type = eta_type,
#'       HTE_type = HTE_type)$data
    
    # Extract HTE_true 
    HTE_true <- test_data$HTE
    
    
    # Compute the estimated HTE
    X_HTE_test <- transform_X(
    single_data = test_data,
    transform_spec = HTE_spec)
  
    test_regressor_HTE <- cbind(1, X_HTE_test)
    p_test_regressor_HTE <- ncol(test_regressor_HTE)
    
    for each row of df_HTE_coef
    {
      beta_HTE is the last p column of df_HTE_coef
       HTE_est <- as.vector(test_regressor_HTE %*% beta_HTE)
        MSE <- mean((HTE_est - HTE_true)^2)
       }
       
       Focus on k = 0 case, i.e., filter k == 0
       
       Key variables in df_HTE_coef: iteration,lasso_type,eta_spec,HTE_spec,prop_score_spec,stage,k
       
       Keep other key variables, and average the MSE over iteration 
       
    }
    aggregated_metrics_n <- df_HTE_coef
    aggregated_metrics_n$n <- n
    if (is.null(aggregated_metrics)){
      aggregated_metrics <- aggregated_metrics_n
    }else{
      aggregated_metrics <- rbind(aggregated_metrics, aggregated_metrics_n)
    }
    
  }# end for n in n_list






source("scripts/TV-CSL/result-process.R")
make_plots_from_json(json_file_lasso = json_file_lasso,
                     json_file_TV_CSL = json_file_TV_CSL, 
                     eta_type = eta_type,
                     HTE_type = HTE_type,
                     output_csv_dir = "scripts/TV-CSL/tables-and-plots")

# source("scripts/TV-CSL/result-process.R")
# make_beta_HTEs_table(
#   json_file = json_file
# ) 

# json_file_TV_CSL <- "scripts/TV-CSL/params-lasso.json"
# process_results_to_csv(
#   "scripts/TV-CSL/params-TV-CSL.json"
# )
