library(jsonlite)
library(here)
library(dplyr)

source("scripts/TV-CSL/result-process.R")

json_file <- "scripts/TV-CSL/params.json"
config <- load_experiment_config(json_file)


aggregated_metrics <- 
  process_all_iterations(
    config=config, 
    results_dir=RESULTS_DIR
  )

n <- config$n
methods <- config$methods
eta_type <- config$eta_type
CATE_type <- config$CATE_type
R <- config$R

is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled

i <- 1
seed_value <- 123 + 11 * i


result_csv_file <- generate_output_path(
  is_running_cox = is_running_cox,
  is_running_lasso = is_running_lasso,
  is_running_TV_CSL = is_running_TV_CSL,
  eta_type = eta_type,
  CATE_type = CATE_type,
  n = n,
  i = i,
  seed_value = seed_value
)

  all_results <- list()
  for (i in 1:R) {
    seed_value <- 123 + 11 * i
    
    result_csv_file <- generate_output_path(
      is_running_cox = is_running_cox,
      is_running_lasso = is_running_lasso,
      is_running_TV_CSL = is_running_TV_CSL,
      eta_type = eta_type,
      CATE_type = CATE_type,
      n = n,
      i = i,
      seed_value = seed_value
    )
    
    
    if (file.exists(result_csv_file)) {
      iteration_result <- read_single_iteration_result(result_csv_file)
      all_results[[i]] <- iteration_result
    } else {
      cat("Warning: File not found for iteration", i, "\n")
    }
  }
  
  combined_results <- do.call(rbind, all_results)

  unique(combined_results$Specification)
  