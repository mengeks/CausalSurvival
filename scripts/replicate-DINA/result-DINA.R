library(jsonlite)
library(here)
library(dplyr)
source("R/result-summaries.R")
source("R/DINA-result-process.R")

## Run a test
process_results_to_csv(
  "scripts/replicate-DINA/config-DINA-test.json"
)

##### 
## linear baselines
#####
process_results_to_csv(
  "scripts/replicate-DINA/log-linear/config-n-200.json"
)
process_results_to_csv(
  "scripts/replicate-DINA/log-linear/config-n-500.json"
)

process_results_to_csv(
  "scripts/replicate-DINA/log-linear/config-n-1000.json"
)




# ##### 
# ## cosine baselines
# #####
# 
# process_results_to_csv(
#   "scripts/replicate-DINA/config-custom-cox-K2.json"
# )
# 
# process_results_to_csv(
#   "scripts/replicate-DINA/config-DINA-log.json"
# )
# 
# process_results_to_csv(
#   "scripts/replicate-DINA/config-DINA-log-n-500.json"
# )
# 
# 

process_results_to_csv(
  "scripts/replicate-DINA/linear-interaction_cosine/config-DINA-n-500.json"
)

