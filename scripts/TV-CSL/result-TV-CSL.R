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
  HTE_spec = "linear"
)

json_file_lasso <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  json_file = json_file_lasso,
  eta_type = eta_type,
  HTE_type = HTE_type,
)






source("scripts/TV-CSL/result-process.R")
make_HTE_by_eta_plots(json_file_lasso = "scripts/TV-CSL/params-lasso.json",
                     json_file_TV_CSL = "scripts/TV-CSL/results/result-TV-CSL-5-Nov-2024/params-TV-CSL.json", 
                     eta_type = "non-linear",
                     HTE_type = "linear",
                     output_csv_dir = "scripts/TV-CSL/tables-and-plots/5-Nov-2024")





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
