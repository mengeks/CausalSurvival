library(jsonlite) # For reading JSON files
source("R/data-reader.R")
source("R/time-varying-estimate.R")
source("R/TV-CSL-runner.R")


run_experiment(
  "scripts/TV-CSL/linear-interaction_cosine/config-n-200.json"
)

