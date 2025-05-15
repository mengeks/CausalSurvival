########
# Heart Transplant Survival Analysis: Fixed vs. Time-Varying Models
########
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

source(here::here("R/tvcsl-package.R"))

# Load and prepare data
df_bwh_raw <- readRDS("data/bwh-heart-transplant/cleaned_bwh_transplant_data.rds")
df_bwh <- df_bwh_raw %>% 
  filter(!is.na(time_to_event)) %>%
  mutate(trt = transplant,
         age_scaled = scale(age))

# Add standardized year 
# Only create if you decide to standardize year in your models
df_bwh <- df_bwh %>%
  mutate(year_scaled = scale(year))

# Update time-varying dataset with standardized year
df_bwh_timevar <- create_time_varying_dataset(
  data = df_bwh,
  event_time = "time_to_event",
  event = "event",
  tx_time = "time_to_tx",
  covariates = c("age", "age_scaled", "ventilation", "year", "year_scaled", "time_to_tx", "bmi"),
  id_col = "subject"
)

# Generate summary statistics by transplant status
df_summary <- df_bwh %>%
  select(event, age, bmi, blood_type, ventilation, support_device, transplant) %>%
  group_by(transplant) %>%
  summarise(across(where(function(x) is.numeric(x) | is.logical(x)), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE)) %>%
  pivot_longer(cols = -transplant, 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(mean|sd)$")

# Visualize timing of transplant treatment
transplant_timing_plot <- ggplot(df_bwh %>% filter(transplant == 1), aes(x = time_to_tx)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Heart Transplant Timing", 
       x = "Months from Listing to Transplant", 
       y = "Count") +
  theme_minimal()
print(transplant_timing_plot)

########
# Model 1: Standard Cox model with fixed treatment
########
fit_fixed <- coxph(Surv(time_to_event, event) ~ (age_scaled + year) * trt, 
                   data = df_bwh, 
                   ties = "breslow")
summary(fit_fixed)

########
# Model 2: Time-varying treatment Cox model
########
fit_timevarying <- coxph(Surv(tstart, tstop, status) ~ (age_scaled + year) * trt,
                         data = df_bwh_timevar, 
                         ties = "breslow")
summary(fit_timevarying)

########
# Model 3: TV-CSL with linear treatment effect
########
df_bwh_timevar$treatment_indicator <- df_bwh_timevar$trt
tvcsl_model_linear <- tvcsl(
  formula = Surv(tstart, tstop, status) ~ age_scaled + year,
  data = df_bwh_timevar,
  treatment_time = "time_to_tx",
  treatment_effect_form = "linear",
  baseline_form = "linear",
  propensity_model = "cox-linear",
  outcome_model = "s-learner",
  final_model_method = "coxph",
  cv_folds = 5,
  id = "subject",
  fast_lasso = TRUE,
  verbose = TRUE
)
summary(tvcsl_model_linear)

########
# Results comparison
########
# Extract treatment effect coefficients
fixed_coefs <- coef(fit_fixed)[grep("trt", names(coef(fit_fixed)))]
timevar_coefs <- coef(fit_timevarying)[grep("trt", names(coef(fit_timevarying)))]
tvcsl_linear_coefs <- tvcsl_model_linear$coefficients

# Create comparison table
results_comparison <- data.frame(
  Model = c("Fixed Cox", "Time-Varying Cox", "TV-CSL Linear"),
  Main_Effect = c(fixed_coefs[1], timevar_coefs[1], tvcsl_linear_coefs[1]),
  stringsAsFactors = FALSE
)

if (length(tvcsl_linear_coefs) > 1) {
  results_comparison$Age_Effect <- c(fixed_coefs[2], timevar_coefs[2], tvcsl_linear_coefs[2])
  results_comparison$Year_Effect <- c(fixed_coefs[3], timevar_coefs[3], tvcsl_linear_coefs[3])
}

print(results_comparison)
