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


# Function to aggregate all CSV files and calculate final metrics
process_all_iterations <- function(config,eta_type, HTE_type, results_dir, n) {
  methods <- config$methods
  # eta_type <- config$eta_type
  # HTE_type <- config$HTE_type
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
                               eta_type,
                               HTE_type, 
                               output_csv_dir = "scripts/TV-CSL/tables-and-plots",
                               file_suffix = "_est_quality.csv" ){
  eta_type_folder_name <- 
    paste0(eta_type, "_", HTE_type)
  
  
  method_setting <- get_method_setting(config)
  
  table_csv_file <- 
    file.path(output_csv_dir, 
              paste0(method_setting, 
                     eta_type_folder_name, 
                     file_suffix ) )
  return(table_csv_file)
}

#' Combine Individual MSE Results to a table by method averages
#' 
#' @description 
#' Takes DGP configuration a JSON configuration file, loop over
#' different sample sizes, gather the estimation results and saves the aggregated metrics to a CSV file.
#' 
#' @param json_file character, Path to the JSON configuration file containing 
#'   experiment parameters
#' @param n_list numeric vector, Sample sizes to process (default: c(200, 500, 1000, 2000))
#' @param output_csv_dir character, Directory path where the output CSV will be saved
#'   (default: "scripts/TV-CSL/tables-and-plots")
#' 
#' @return None (invisible). Writes results to a CSV file and prints confirmation message.
#' 
#' @details 
#' The function performs the following steps:
#' 1. Loads experiment configuration from JSON
#' 2. Processes results for each sample size in n_list
#' 3. Combines results into a single data frame
#' 4. Saves aggregated results to CSV
#' 
#' @dependencies 
#' Requires load_experiment_config(), process_all_iterations(), and get_table_csv_file()
#' 
#' @examples 
#' \dontrun{
#' process_results_to_csv(
#'   json_file = "path/to/config.json",
#'   n_list = c(100, 300, 500),
#'   output_csv_dir = "output/directory"
#' )
#' }
#' 
#' @export
process_results_to_csv <- function(json_file,
                                   eta_type,
                                   HTE_type,
                                   n_list = c(200, 500, 1000, 2000),
                                   output_csv_dir = "scripts/TV-CSL/tables-and-plots") {
  
  config <- load_experiment_config(json_file)
  
  aggregated_metrics <- NULL
  
  for (n in n_list){
    aggregated_metrics_n <- 
      process_all_iterations(
        config=config, 
        eta_type = eta_type,
        HTE_type = HTE_type,
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
                                       eta_type = eta_type,
                                       HTE_type = HTE_type,
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


load_and_process_table_data <- function(json_file,
                                        eta_type,
                                        HTE_type,
                                        output_csv_dir) {
  config <- load_experiment_config(json_file)
  table_csv_file <- get_table_csv_file(config,eta_type, HTE_type, output_csv_dir)
  data <- read.csv(table_csv_file)
  data <- data %>%
    mutate(Specification = as.character(Specification),
           Model = sapply(strsplit(Specification, "_"), `[`, 1),
           eta_model = sapply(strsplit(Specification, "_"), `[`, 2),
           HTE_model = sapply(strsplit(Specification, "_"), `[`, 3))
  return(data)
}


### TODO: 
### 0. Make the file name better
### 1. Remove "regressor-spec" from eta_model label
### 2. Remove "HTE-spec" from eta_model label
### input: the table
make_plots_from_json <- function(json_file_lasso,
                                 json_file_TV_CSL, 
                                 eta_type,
                                 HTE_type,
                                 output_csv_dir = "scripts/TV-CSL/tables-and-plots") {
  
  data_lasso <- load_and_process_table_data(json_file_lasso,eta_type,HTE_type, output_csv_dir)
  data_TV_CSL <- load_and_process_table_data(json_file_TV_CSL,eta_type,HTE_type, output_csv_dir)
  
  combined_data <- bind_rows(data_lasso, data_TV_CSL, .id = "source")
  
  # Create custom labeling functions for each factor
  hte_labels <- c(
    "HTE-spec-complex" = "Treatment effect: overly complex",
    "HTE-spec-linear" = "Treatment effect: correctly specified"
  )
  
  eta_labels <- c(
    "regressor-spec-complex" = "Baseline: Mildly mis-specified",
    "regressor-spec-linear" = "Baseline: Quite mis-specified"
  )
  
  p <- ggplot(combined_data, aes(x = as.factor(n), y = MSE, color = Method)) +
    geom_point() +
    geom_line() +
    # facet_grid(HTE_model ~ eta_model, labeller = label_both) +
    facet_grid(
      HTE_model ~ eta_model,
      labeller = labeller(
        HTE_model = hte_labels,
        eta_model = eta_labels
      )
    ) + 
    labs(x = "n", y = "MSE", title = "MSE by Method and Model Specifications") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          strip.text = element_text(face = "bold"))
  
  output_plot_path <- 
    file.path(output_csv_dir, 
              paste0("both-methods_", 
                       paste0(eta_type, "_", HTE_type), 
                     "_MSE_plots.png") )
  ggsave(output_plot_path, plot = p, width = 8, height = 6)
  
  return(output_plot_path)
}

### TODO: 
### 0. Make the file name better
### 1. Remove "regressor-spec" from eta_model label
### 2. Remove "HTE-spec" from eta_model label
### input: the table
make_plots_from_json_single_method <- function(json_file, 
                                 output_csv_dir = "scripts/TV-CSL/tables-and-plots") {
  
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
    eta_type = eta_type, 
    HTE_type = HTE_type, 
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
