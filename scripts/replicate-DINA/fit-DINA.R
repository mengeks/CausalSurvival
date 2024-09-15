library(jsonlite) # For reading JSON files
source("R/data-reader.R")
source("R/non-time-varying-estimate.R")
source("R/DINA-runner.R")

# ## run the whole experiment
# run_experiment(
#   "scripts/replicate-DINA/config-DINA-n-200-R-200.json"
# )

## run the experiment of DINA, K = 2
run_experiment(
  "scripts/replicate-DINA/config-custom-cox-K2.json"
)
# about one minute
# 
# ## run the experiment of DINA, K = 8
# run_experiment(
#   "scripts/replicate-DINA/config-custom-cox-K8.json"
# )
# # about one minute


