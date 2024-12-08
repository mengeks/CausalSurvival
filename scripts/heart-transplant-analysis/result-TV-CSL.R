library(jsonlite)
library(here)
library(dplyr)
source("scripts/heart-transplant-analysis/result-process.R")
source("scripts/heart-transplant-analysis/time-varying-estimate.R")

source("scripts/heart-transplant-analysis/result-process.R")
json_file <- "scripts/heart-transplant-analysis/params-heart-transplant.json"
results <- evaluate_HTE_metrics(
  json_file = json_file,
  results_dir = "scripts/heart-transplant-analysis/results/",
  HTE_spec = "linear",
  output_csv_dir = "scripts/heart-transplant-analysis/tables-plots/"
)






make_tables_heart_transplant <- function(
    MSE_csv_dir = "scripts/heart-transplant-analysis/tables-plots/est_quality.csv") {
  
  source("scripts/heart-transplant-analysis/result-process.R")
  MSE_data <- load_and_process_table_data_multi_spec(MSE_csv_dir = MSE_csv_dir)
  data_TV_CSL <- MSE_data %>% 
    filter(method == "TV-CSL")
  data_lasso <- MSE_data %>% 
    filter(method == "single") %>%
    mutate(Method = method)
  data_TV_CSL$Method <- paste(data_TV_CSL$method, data_TV_CSL$lasso_type, sep = "_")
  combined_data <- rbind(data_lasso, data_TV_CSL)
  combined_data_selected  <- combined_data %>% 
    filter(k == 0, stage == "final") %>%
    select(Method, eta_spec,MSE, mean_sq_HTE_true)  %>%
    mutate(R_sq = 1 - MSE / mean_sq_HTE_true)
  
  
  library(dplyr)
  library(tidyr)
  library(knitr)
  library(kableExtra)
  
  # Rename `eta_spec` for clarity
  combined_data_selected <- combined_data_selected %>%
    rename(`Baseline Outcome Model` = eta_spec)
  
  # Reshape data: `Method` as rows, `Baseline Outcome Model` as columns
  latex_table_data <- combined_data_selected %>%
    select(Method, `Baseline Outcome Model`, MSE) %>%
    mutate(MSE = signif(MSE, 3)) %>%
    pivot_wider(
      names_from = `Baseline Outcome Model`, 
      values_from = MSE
    ) 
  
  # Create a LaTeX table
  latex_table <- latex_table_data %>%
    kbl(
      format = "latex", 
      booktabs = TRUE, 
      caption = "MSE by Method and Baseline Outcome Model"
    ) %>%
    kable_styling(latex_options = c("hold_position"))
  
  # Print LaTeX code
  cat(latex_table)
  
  # hte_labels <- c(
  #   "complex" = "Treatment effect: overly complex",
  #   "linear" = "Treatment effect: correctly specified"
  # )
  # 
  # eta_labels <- c(
  #   "complex" = "Baseline: Mildly mis-specified",
  #   "linear" = "Baseline: Quite mis-specified"
  # )
  # 
  # prop_score_labels <- c(
  #   "cox-linear-censored-only" = "Prop score: correctly specified",
  #   "cox-linear-mis-specification" = "Prop score: mis-specified"
  # )
  return(output_plot_path)
}








source("scripts/TV-CSL/result-process.R")
make_HTE_by_eta_plots(json_file_lasso = json_file_lasso,
                     json_file_TV_CSL = json_file_TV_CSL, 
                     eta_type = eta_type,
                     HTE_type = HTE_type,
                     output_csv_dir = "scripts/TV-CSL/tables-and-plots")





process_results_to_csv(
  json_file =  json_file_TV_CSL,
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
)



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
