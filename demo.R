library(survival)
library(dplyr)

# Source the package
source("R/tvcsl-package.R")

# Generate some simulated data
sim_data <- generate_simple_data(n = 200)

# Create time-varying dataset
data_tv <- create_time_varying_dataset(
  data = sim_data,
  event_time = "time_to_event",
  event = "event",
  tx_time = "time_to_tx",
  covariates = c("age", "comorbidity", "biomarker","time_to_tx"),
  id = "id"
)


# Fit a TV-CSL model
model <- tvcsl(
  formula = "surv(tstart, tstop, event) ~ age + comorbidity + biomarker",
  data = data_tv,
  treatment_time = "time_to_tx",
  treatment_effect_form = "linear",
  baseline_form = "linear",
  propensity_model = "cox-linear",
  outcome_model = "s-learner",
  final_model_method = "coxph",
  cv_folds = 5,
  id = id,
  fast_lasso = TRUE
)

# View results
summary(model)