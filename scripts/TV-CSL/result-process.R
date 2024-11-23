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

get_is_running_flags <- function(methods) {
  list(
    is_running_cox = !is.null(methods$cox) && methods$cox$enabled,
    is_running_lasso = !is.null(methods$lasso) && methods$lasso$enabled,
    is_running_TV_CSL = !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  )
}

# Function to aggregate all CSV files and calculate final metrics
process_all_iterations <- function(config,eta_type, HTE_type, results_dir, n) {
  methods <- config$methods
  # eta_type <- config$eta_type
  # HTE_type <- config$HTE_type
  R <- config$R
  
  running_flags <- get_is_running_flags(methods)
  
  is_running_cox <- running_flags$is_running_cox
  is_running_lasso <- running_flags$is_running_lasso
  is_running_TV_CSL <- running_flags$is_running_TV_CSL

  # is_running_cox <- !is.null(methods$cox) && methods$cox$enabled
  # is_running_lasso <- !is.null(methods$lasso) && methods$lasso$enabled
  # is_running_TV_CSL <- !is.null(methods$TV_CSL) && methods$TV_CSL$enabled
  
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

evaluate_HTE_metrics <- function(json_file, results_dir, eta_type, HTE_type, n_list, HTE_spec) {
  # Load the experiment configuration
  config <- load_experiment_config(json_file)
  methods <- config$methods
  
  # Get running flags
  running_flags <- get_is_running_flags(methods)
  is_running_cox <- running_flags$is_running_cox
  is_running_lasso <- running_flags$is_running_lasso
  is_running_TV_CSL <- running_flags$is_running_TV_CSL
  
  # Initialize aggregated metrics
  aggregated_metrics <- NULL
  
  for (n in n_list) {
    # Generate method settings and output folder
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
    
    # Path to the HTE coefficient file
    HTE_coef_file <- paste0("HTE-spec-", HTE_spec, "_beta-HTE.csv")
    result_csv_file <- file.path(output_folder, HTE_coef_file)
    
    # Check if the file exists
    if (!file.exists(result_csv_file)) {
      cat("Warning: File not found for n =", n, "\n")
      next
    }
    
    # Load HTE coefficient data
    df_HTE_coef <- read.csv(result_csv_file)
    
    
    
    # Read a single test set
    test_data <- read_single_simulation_data(
      n = 2000, 
      i = 101, 
      eta_type = eta_type,
      HTE_type = HTE_type
    )$data
    
    # Extract HTE_true and transform test data
    HTE_true <- test_data$HTE
    X_HTE_test <- transform_X(
      single_data = test_data,
      transform_spec = HTE_spec
    )
    test_regressor_HTE <- cbind(1, X_HTE_test)
    # print(paste0("dim(test_regressor_HTE)=",dim(test_regressor_HTE)))
    
    p_regressor_HTE <- ncol(test_regressor_HTE)
    n_col_df <- ncol(df_HTE_coef)
    
    
    ## Compute the first stage average
    compute_first_stage_average <- function(df_HTE_coef, p_regressor_HTE, n_col_df) {
      
      coefficient_colnames <- colnames(df_HTE_coef)[(n_col_df - p_regressor_HTE + 1):n_col_df]
      # print(coefficient_colnames)
      
      first_stage_df <- df_HTE_coef %>% 
        filter(stage == "first")
      
      first_stage_avg <- first_stage_df %>%
        group_by(iteration, lasso_type, eta_spec, HTE_spec, prop_score_spec, stage) %>%
        summarise(across(all_of(coefficient_colnames), mean, na.rm = TRUE)) %>%
        ungroup()
      
      first_stage_avg$k <- 0
      return(first_stage_avg)
    }
    
    first_stage_avg <- 
      compute_first_stage_average(df_HTE_coef=df_HTE_coef, 
                                  p_regressor_HTE=p_regressor_HTE,
                                  n_col_df = n_col_df)
    
    # print(head(first_stage_avg))
    # print(head(df_HTE_coef))
    df_HTE_coef <- rbind(df_HTE_coef,first_stage_avg)
    
    
    # Compute MSE for each row of df_HTE_coef
    df_HTE_coef$MSE <- apply(df_HTE_coef, 1, function(row) {
      # print(paste0("length(row)  = ", length(row) ))
      # print(paste0(" ncol(test_regressor_HTE) + 1 = ",  ncol(test_regressor_HTE) + 1) )
      # beta_HTE <- as.numeric(row[(ncol(row) - ncol(test_regressor_HTE) + 1):ncol(row)])
      beta_HTE <- as.numeric(row[(n_col_df - p_regressor_HTE + 1):n_col_df])
      HTE_est <- as.vector(test_regressor_HTE %*% beta_HTE)
      mean((HTE_est - HTE_true)^2)
    })
    
    # # Filter for k == 0
    # df_HTE_coef <- df_HTE_coef[df_HTE_coef$k == 0, ]
    
    print(paste0("n = ", n))
    print("print((df_HTE_coef %>% filter(iteration == 65)))")
    print((df_HTE_coef %>% filter(iteration == 65)))
    
    
    aggregated_metrics_n <- df_HTE_coef %>%
      group_by(lasso_type, eta_spec, HTE_spec, prop_score_spec, stage, k) %>%
      summarise(MSE = mean(MSE)) %>%
      # summarise(MSE = mean(MSE, na.rm = TRUE)) %>%
      ungroup()
    
    # Add n as a variable
    aggregated_metrics_n$n <- n
    
    # Combine with overall aggregated metrics
    if (is.null(aggregated_metrics)) {
      aggregated_metrics <- aggregated_metrics_n
    } else {
      aggregated_metrics <- rbind(aggregated_metrics, aggregated_metrics_n)
    }
  }
  
  return(aggregated_metrics)
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

