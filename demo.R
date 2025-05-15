library(survival)
library(dplyr)

# Source the package
source("R/tvcsl-package.R")

# Generate some simulated data
set.seed(123)
sim_data <- generate_simple_data(n = 500)

# Create time-varying dataset
data_tv <- create_time_varying_dataset(
  data = sim_data,
  event_time = "time_to_event",
  event = "event",
  tx_time = "time_to_tx",
  covariates = c("age", "comorbidity", "biomarker", "time_to_tx"),
  id_col = "id"  # Changed from id to id_col
)

# Fit a TV-CSL model
model <- tvcsl(
  formula = Surv(tstart, tstop, status) ~ age + comorbidity + biomarker,  # Fixed Surv() and used formula object
  data = data_tv,
  treatment_time = "time_to_tx"
)

# View results
summary(model)

new_patients <- data.frame(
  age = c(45, 65),
  comorbidity = c(2, 3),
  biomarker = c(1.2, 0.8)
)
predict(model, new_patients)
## TODO: the prediction is the same -- need to fix this