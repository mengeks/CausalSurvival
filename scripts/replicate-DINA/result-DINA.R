library(jsonlite)
library(here)
library(dplyr)
source("R/result-summaries.R")
source("R/DINA-result-process.R")

process_results_to_csv(
  "scripts/replicate-DINA/config-custom-cox-K2.json"
)

process_results_to_csv(
  "scripts/replicate-DINA/config-custom-cox-K8.json"
)

