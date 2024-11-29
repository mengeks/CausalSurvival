library(jsonlite)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(here))

source("R/data-handler.R")
source("scripts/heart-transplant-analysis/time-varying-estimate.R")


#' Run a Single Iteration of the Experiment and Save Results to CSV
#'
#' This function runs the experiment for a single iteration, saves the estimation results to a CSV file, 
#' and also saves the true parameters used in the simulation.
#'
#' @param i The iteration number to run.
#' @param json_file Path to the JSON configuration file.
#' @param verbose The level of verbosity (0 = default, 1 = progress info, 2 = detailed info).
run_experiment_iteration <- 
  function(i = 1, 
           json_file = "scripts/heart-transplant-analysis/params-heart-transplant.json", 
           results_dir = "scripts/heart-transplant-analysis/results", 
           verbose = 0) {
    
    if (verbose >= 1) 
      message("Running iteration ", i)
    
    config <- fromJSON(json_file)
    
    methods <- config$methods
    # K <- ifelse(is.null(config$K), 5, config$K)
    K <- 2
    
    seed_value <- 123 + 11 * i
    set.seed(seed_value)
    
    input_dir <- here("data", "heart-transplant-sim")
    
    
    start_time <- Sys.time()
    
    
    read_single_sim <- function(i, input_dir){
      file_name <- paste0("sim_data_", i, ".rds")
      
      file_path <- file.path(input_dir, file_name)
      
      if (!file.exists(file_path)) {
        stop(paste("File does not exist:", file_path))
      }
      
      loaded_data <- readRDS(file_path)
      return(loaded_data)
    }
    
    single_data <- read_single_sim(i = i, 
                                   input_dir = input_dir)$data
    
    test_data <- read_single_sim(i = i + 100, 
                                 input_dir = input_dir)$data
    end_time <- Sys.time()
    
    time_taken_lasso <- mse_estimates_lasso <- list()
    is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
    if (is_running_lasso) {
      start_time <- Sys.time()
      source("scripts/heart-transplant-analysis/time-varying-estimate.R")
      lasso_results <- 
        run_lasso_estimation(
          single_data = single_data,
          test_data = test_data,
          i = i,
          methods_lasso = methods$lasso
        )
      # train_data <- 
      #   preprocess_data(single_data, 
      #                   run_time_varying = T)
      end_time <- Sys.time()
      
      for (config_name in names(lasso_results)) {
        time_taken_lasso[[config_name]] <- 
          lasso_results[[config_name]]$time_taken
        mse_estimates_lasso[[config_name]] <- lasso_results[[config_name]]$MSE
      }
      
      if (verbose >= 1) 
        message("Time to run lasso model: ", 
                as.numeric(difftime(end_time, start_time, units = "secs")), " seconds")
      if (verbose >= 2) {
        message("Lasso Results:")
        print(mse_estimates_lasso)
      }
    }
    
    
    result_df_lasso <- data.frame(
      Method = rep("Lasso", length(mse_estimates_lasso)),
      Specification = names(mse_estimates_lasso),
      MSE_Estimate = unlist(mse_estimates_lasso),
      Time_Taken = unlist(time_taken_lasso)
    )
    
    
    mse_estimates_TV_CSL <- list()
    time_taken <- list()
    is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
    if (is_running_TV_CSL) {
      start_time <- Sys.time()
      
      TV_CSL_results <- 
        run_TV_CSL_estimation(
          train_data_original = single_data, 
          test_data = test_data,
          methods_TV_CSL = methods$TV_CSL,
          i = i,
          K = 2
          # HTE_type = HTE_type,
          # eta_type = eta_type
        )
      
      end_time <- Sys.time()
      
      for (config_name in names(TV_CSL_results)) {
        time_taken[[config_name]] <- TV_CSL_results[[config_name]]$time_taken
        mse_estimates_TV_CSL[[config_name]] <- TV_CSL_results[[config_name]]$MSE
      }
      
      if (verbose >= 1)
        message("Time to run TV_CSL model: ",
                as.numeric(difftime(end_time, start_time, units = "secs")), " seconds")
      if (verbose >= 2) {
        message("TV_CSL Results:")
        print(mse_estimates_TV_CSL)
      }
    }
    
    result_df_TV_CSL <- data.frame(
      Method = rep("TV_CSL", length(mse_estimates_TV_CSL)),
      Specification = names(mse_estimates_TV_CSL),
      MSE_Estimate = unlist(mse_estimates_TV_CSL),
      Time_Taken = unlist(time_taken)
    )
    
    
    result_df <- 
      rbind(result_df_lasso,
            result_df_TV_CSL)
    
    
    # result_csv_file <- generate_output_path(
    #   results_dir = results_dir, 
    #   is_running_lasso = is_running_lasso,
    #   is_running_TV_CSL = is_running_TV_CSL,
    #   eta_type = eta_type,
    #   HTE_type = HTE_type,
    #   n = n,
    #   i = i,
    #   seed_value = seed_value
    # )
    # 
    # 
    # write.csv(result_df, result_csv_file, row.names = FALSE)
    # 
    
    if (verbose >= 1) {
      message("Results for iteration ", i, " saved to ", result_csv_file)
    }
  }

# ## Example
# source("scripts/heart-transplant-analysis/time-varying-estimate.R")
# run_experiment_iteration(i = 1, json_file = "scripts/heart-transplant-analysis/params-heart-transplant.json", 
#          results_dir = "scripts/heart-transplant-analysis/results", 
#          verbose = 0)
