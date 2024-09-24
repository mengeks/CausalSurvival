library(jsonlite)
source("R/data-reader.R")
source("R/non-time-varying-estimate.R")
source("R/DINA-runner.R")

# ## run a test
run_experiment(
  "scripts/replicate-DINA/config-DINA-test.json"
)
##### 
## linear baselines
#####
run_experiment(
  "scripts/replicate-DINA/log-linear/config-n-200.json"
)
run_experiment(
  "scripts/replicate-DINA/log-linear/config-n-500.json"
)
run_experiment(
  "scripts/replicate-DINA/log-linear/config-n-1000.json"
)



##### 
## cosine baselines
#####
## The two experiments below were to compare two things 
## 1. n = 200 vs n = 500
## 2. Whether slasso's good performance is due to 
#     easy outcome (linear-interaction)

# ## run the whole experiment for log, n-200
# run_experiment(
#   "scripts/replicate-DINA/config-DINA-log.json"
# )
# 
# ## run the whole experiment for log, n-500
# run_experiment(
#   "scripts/replicate-DINA/config-DINA-log-n-500.json"
# )



# # Run for n = 500
# run_experiment(
#   "scripts/replicate-DINA/linear-interaction_cosine/config-DINA-n-500.json"
# )


## Below two: 
#   We tune final_model_method to custom_cox
#   We compare K == 2 vs K == 8
# ## run the experiment of DINA, K = 2
# run_experiment(
#   "scripts/replicate-DINA/linear-interaction_cosine/config-custom-cox-K2.json"
# )
# # about one minute
# 
# ## run the experiment of DINA, K = 8
# run_experiment(
#   "scripts/replicate-DINA/linear-interaction_cosine/config-custom-cox-K8.json"
# )
# # about one minute



# ## run the whole experiment
## This is $n = 200, R = 200$. 
#  For DINA, only run 
#     - final_model_method == "coxph"
#     - K == 2
# Table: the bias table
# Takeaway: DINA runs but did not beat slasso
# run_experiment(
#   "scripts/replicate-DINA/linear-interaction_cosine/config-DINA-n-200-R-200.json"
# )

