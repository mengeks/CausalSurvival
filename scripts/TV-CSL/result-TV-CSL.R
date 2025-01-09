library(jsonlite)
library(here)
library(dplyr)
source("scripts/TV-CSL/result-process.R")
source("scripts/TV-CSL/time-varying-estimate.R")

eta_type <- "non-linear"; HTE_type <- "linear"


## Gather table for Figure 1
source("scripts/TV-CSL/result-process.R")
json_file_TV_CSL <- "scripts/TV-CSL/params-TV-CSL.json"
results <- evaluate_HTE_metrics(
  json_file = json_file_TV_CSL,
  results_dir = "scripts/TV-CSL/results/",
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
  HTE_spec = "linear"
)




## Gather table for Figure 2
json_file_lasso <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  json_file = json_file_lasso,
  eta_type = eta_type,
  HTE_type = HTE_type,
  output_csv_dir = "scripts/TV-CSL/tables-and-plots/5-Nov-2024"
)

json_file_TV_CSL <- "scripts/TV-CSL/results/result-TV-CSL-5-Nov-2024/params-TV-CSL.json"
process_results_to_csv(
  json_file =  json_file_TV_CSL,
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000, 2000),
  output_csv_dir = "scripts/TV-CSL/tables-and-plots/5-Nov-2024",
  results_dir = "scripts/TV-CSL/results/result-TV-CSL-5-Nov-2024/"
)


