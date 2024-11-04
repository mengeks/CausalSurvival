source("R/data-handler.R")
library(jsonlite)
library(ggplot2)
library(dplyr)

#' Load Experiment Configuration
#'
#' Loads the experiment configuration from a JSON file.
#' 
#' @param json_file The path to the JSON configuration file.
#' @return A list containing the experiment configuration.
load_experiment_config <- function(json_file) {
  config <- fromJSON(json_file)
  return(config)
}


# Helper function to read individual CSV files
read_single_iteration_result <- function(csv_file) {
  df <- read.csv(csv_file)
  return(df)
}

# generate_result_csv_file <- function(results_dir, eta_type_folder_name, n, i, seed_value) {
#   result_csv_file <- paste0(
#     results_dir, 
#     "/", eta_type_folder_name, 
#     "-n_", n, 
#     "-iteration_", i, 
#     "-seed_", seed_value, ".csv"
#   )
#   return(result_csv_file)
# }





# Function to aggregate all CSV files and calculate final metrics
process_all_iterations <- function(config, results_dir, n) {
  methods <- config$methods
  eta_type <- config$eta_type
  HTE_type <- config$HTE_type
  R <- config$R
  
  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled

  
  all_results <- list()
  all_times <- list()
  
  for (i in 1:R) {
    seed_value <- 123 + 11 * i
    
    result_csv_file <- generate_output_path(
      is_running_cox = is_running_cox,
      is_running_lasso = is_running_lasso,
      is_running_TV_CSL = is_running_TV_CSL,
      eta_type = eta_type,
      HTE_type = HTE_type,
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
  combined_results$Specification <- 
    gsub("S_lasso", "S-lasso", combined_results$Specification)
  combined_results$Specification <- 
    gsub("T_lasso", "T-lasso", combined_results$Specification)
  
  
  aggregated_metrics <- combined_results %>%
    group_by(Method, Specification) %>%
    summarise(
      MSE = mean(MSE_Estimate)
    )
  
  return(aggregated_metrics)
}


get_method_setting <- function(config){
  methods <- config$methods
  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  method_setting <- paste0(
    ifelse(is_running_cox, "cox_", ""),
    ifelse(is_running_lasso, "lasso_", ""),
    ifelse(is_running_TV_CSL, "TV-CSL_", "")
  )
  return(method_setting)
}

get_table_csv_file <- function(config, 
                               output_csv_dir = "scripts/TV-CSL/tables-and-plots",
                               file_suffix = "_est_quality.csv" ){
  eta_type_folder_name <- 
    paste0(config$eta_type, "_", config$HTE_type)
  
  
  method_setting <- get_method_setting(config)
  
  table_csv_file <- 
    file.path(output_csv_dir, 
              paste0(method_setting, 
                     eta_type_folder_name, 
                     file_suffix ) )
  return(table_csv_file)
}

process_results_to_csv <- function(json_file, 
                                   n_list = c(200, 500, 1000, 2000),
                                   output_csv_dir = "scripts/TV-CSL/tables-and-plots") {
  
  config <- load_experiment_config(json_file)
  
  aggregated_metrics <- NULL
  
  for (n in n_list){
    aggregated_metrics_n <- 
      process_all_iterations(
        config=config, 
        results_dir=RESULTS_DIR,
        n = n
      )
    aggregated_metrics_n$n <- n
    if (is.null(aggregated_metrics)){
      aggregated_metrics <- aggregated_metrics_n
    }else{
      aggregated_metrics <- rbind(aggregated_metrics, aggregated_metrics_n)
    }
  }
  
  
  table_csv_file <- get_table_csv_file(config = config,
                                       output_csv_dir = output_csv_dir)
  
  write.csv(
    aggregated_metrics, 
    table_csv_file, 
    row.names = FALSE)
  
  cat("Aggregated results saved to", table_csv_file, "\n")
}

get_plot_file <- function(config, 
                          output_csv_dir = "scripts/TV-CSL/tables-and-plots"){
  eta_type_folder_name <- 
    paste0(config$eta_type, "_", config$HTE_type)
  
  
  method_setting <- get_method_setting(config)
  
  plot_file <- 
    file.path(output_csv_dir, 
              paste0(method_setting, 
                     eta_type_folder_name, 
                     "_MSE_plots.png") )
  return(plot_file)
}

### TODO: 
### 0. Make the file name 
### 1. Remove "regressor-spec" from eta_model label
### 2. Remove "HTE-spec" from eta_model label
make_plots_from_json <- function(json_file, output_csv_dir = "scripts/TV-CSL/tables-and-plots") {
  
  config <- load_experiment_config(json_file)
  table_csv_file <- get_table_csv_file(config, output_csv_dir)
  data <- read.csv(table_csv_file)
  
  data <- data %>%
    mutate(Specification = as.character(Specification),
           Model = sapply(strsplit(Specification, "_"), `[`, 1),
           eta_model = sapply(strsplit(Specification, "_"), `[`, 2),
           HTE_model = sapply(strsplit(Specification, "_"), `[`, 3))
  
  p <- ggplot(data, aes(x = as.factor(n), y = MSE, color = Model)) +
    geom_point() +
    geom_line() +
    facet_grid(HTE_model ~ eta_model, labeller = label_both) +
    labs(x = "n", y = "MSE", title = "MSE by Model, eta model, and HTE model") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          strip.text = element_text(face = "bold"))
  
  output_plot_path <- get_plot_file(config = config, 
                                    output_csv_dir = output_csv_dir)
  ggsave(output_plot_path, plot = p, width = 8, height = 6)
  
  return(output_plot_path)
}


make_beta_HTEs_table <- function(json_file, 
                                 n_list = c(200, 500, 1000, 2000), 
                                 output_csv_dir = "scripts/TV-CSL/tables-and-plots",
                                 results_dir = RESULTS_DIR) {
  
  config <- load_experiment_config(json_file)
  methods <- config$methods
  eta_type <- config$eta_type
  HTE_type <- config$HTE_type
  
  is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  method_setting <- paste0(
    ifelse(is_running_cox, "cox_", ""),
    ifelse(is_running_lasso, "lasso_", ""),
    ifelse(is_running_TV_CSL, "TV-CSL_", "")
  )
  
  beta_HTEs <- NULL
  for (n in n_list) {
    output_folder_n <- generate_output_folder(
      results_dir = results_dir,
      method_setting = method_setting, 
      eta_type = eta_type, 
      HTE_type = HTE_type, 
      n = n
    )
    
    beta_HTE_n_path <- file.path(output_folder_n, "beta-HTE.csv")
    print(beta_HTE_n_path)
    if (file.exists(beta_HTE_n_path)) {
      beta_HTE_n <- read_csv(beta_HTE_n_path)
      beta_HTE_n$n <- n
      if (is.null(beta_HTEs)) {
        beta_HTEs <- beta_HTE_n
      } else {
        beta_HTEs <- bind_rows(beta_HTEs, beta_HTE_n)
      }
    }
    
  }
  
  print(head(beta_HTEs))
  
  beta_HTEs_summary <- beta_HTEs %>%
    group_by(config_name, n) %>%
    summarize(across(-iteration, mean, na.rm = TRUE), .groups = "drop")
  
  beta_HTEs_table_csv_file <- get_table_csv_file(
    config = config, 
    output_csv_dir = output_csv_dir,
    file_suffix = "_beta_HTEs.csv"
  )
  write_csv(beta_HTEs_summary, beta_HTEs_table_csv_file)
  
  return(beta_HTEs_table_csv_file)
}

# 
# # TODO: make process_results_to_csv explicitly takes into eta_type and HTE_type
# process_results_to_csv_single_n <- function(json_file, n = 500) {
#   
#   config <- load_experiment_config(json_file)
#   
#   
#   aggregated_metrics <- 
#     process_all_iterations(
#       config=config, 
#       results_dir=RESULTS_DIR,
#       n = n
#     )
#   
#   output_csv_dir <- "scripts/TV-CSL/tables"
#   
#   eta_type_folder_name <- 
#     paste0(config$eta_type, "_", config$HTE_type)
#   
#   methods <- config$methods
#   is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
#   is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
#   is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
#   method_setting <- paste0(
#     ifelse(is_running_cox, "cox_", ""),
#     ifelse(is_running_lasso, "lasso_", ""),
#     ifelse(is_running_TV_CSL, "TV-CSL_", "")
#   )
#   
#   metrics_csv_file <- 
#     file.path(output_csv_dir, 
#               paste0(method_setting, eta_type_folder_name,"_n_", n, "_est_quality.csv") )
#   
#   write.csv(
#     aggregated_metrics, 
#     metrics_csv_file, 
#     row.names = FALSE)
#   
#   cat("Aggregated results saved to", metrics_csv_file, "\n")
# }
