library(jsonlite)
library(here)
library(dplyr)
source("scripts/TV-CSL/result-process.R")

# TODO: loop eta_type and HTE_type after process_results_to_csv explicitly takes into them
eta_type <- "non-linear"; HTE_type <- "linear"

json_file_lasso <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  json_file = json_file_lasso,
  eta_type = eta_type,
  HTE_type = HTE_type,
)

## Gather table for TV-CSL
json_file_TV_CSL <- "scripts/TV-CSL/params-TV-CSL-new.json"
process_results_to_csv(
  json_file =  json_file_TV_CSL,
  eta_type = eta_type,
  HTE_type = HTE_type,
  n_list = c(200, 500, 1000,2000),
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

json_file_TV_CSL <- "scripts/TV-CSL/params-lasso.json"
process_results_to_csv(
  "scripts/TV-CSL/params-TV-CSL.json"
)
