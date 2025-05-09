########
# Load and prepare the data
########
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
df_bwh_raw <- 
  readRDS("data/bwh-heart-transplant/cleaned_bwh_transplant_data.rds")
df_bwh <- df_bwh_raw %>% 
  filter(!is.na(time_to_event)) %>%
  mutate(trt = transplant)
source(here::here("R/process-data.R"))
df_bwh_timevar <- 
  create_time_varying_dataset(
    data = df_bwh,
    event_time = "time_to_event",
    event = "event",
    tx_time = "time_to_tx",
    covariates = c("age", "ventilation", "year"),
    id_col = "subject"
  )


########
# Summary statistics
########
df_summary <- df_bwh %>%
  select(event, age,  bmi, blood_type, ventilation, support_device, transplant) %>%
  group_by(transplant) %>%
  summarise(across(where(function(x) is.numeric(x) | is.logical(x)), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE)) %>%
  # Change the separator pattern to avoid splitting support_device incorrectly
  pivot_longer(cols = -transplant, 
               names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(mean|sd)$")

# Print the table
print(xtable(df_summary, caption = "Summary statistics by transplant status"), 
      include.rownames = FALSE)

# We can see that the covariates are quite comparable among these two groups
#  for age and bmi. 

########
# 📊 Visualization: Timing of Treatment
########
## TODO: here we just use transplant as the treatment variable
##      Below i use trt for the treatment variable in the time-varying case. 
##      How to unify them?
ggplot(df_bwh %>% 
         filter(transplant == 1), 
       aes(x = time_to_tx)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Heart Transplant Timing", x = "Months from Listing to Transplant", y = "Count") +
  theme_minimal()

########
# Comparing Fixed Treatment Cox
########
fit_fixed <- 
  coxph(Surv(time_to_event, event) ~ (age + ventilation + year) * trt, 
        data = df_bwh, 
        ties = "breslow")
summary(fit_fixed)


########
# Time-Varying Treatment 
########
### Model: Time-Varying Treatment Cox
fit_timevarying <- 
  coxph(Surv(tstart, tstop, status) ~ (age + ventilation + year) * trt,
        data = df_bwh_timevar, 
        ties = "breslow")
summary(fit_timevarying)

##### 
### Fit TV-CSL
#####
# The main implementation of TV-CSL
source(here::here("R/TV-CSL.R"))
TV_CSL_ret_linear_eta <- 
  TV_CSL(train_data = df_bwh_timevar, 
         test_data = df_bwh, 
         train_data_original = df_bwh, 
         K = 2, 
         prop_score_spec = "cox-linear-all-data",
         lasso_type = "S-lasso",
         eta_type = "non-linear",
         HTE_type = "linear",
         regressor_spec = "complex",
         HTE_spec = "linear",
         final_model_method = "lasso_coxph",
         id_var = "subject",
         lasso_warmstart = F,
         verbose = 0) 

TV_CSL_ret_linear_eta$beta_HTE
cox_ret$beta_HTE
