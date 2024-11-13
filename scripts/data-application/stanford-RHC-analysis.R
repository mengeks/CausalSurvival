library(survival)
library(dplyr)
library(tidyr)
library(psych)
library(xtable)
library(ggplot2)
library(here)
source("scripts/TV-CSL/time-varying-estimate.R")
load(file=here("scripts/data-application/data/stanford-RHC-processed.rds"))

## EDA
# 1. summary stats of the regressors
summary_table <- df_original %>%
  select(age, surgery, year, trt) %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.))))

# Reshape the table to a more readable format for LaTeX
summary_table_tidy <- summary_table %>%
  pivot_longer(cols = everything(), 
               names_to = c("Variable", ".value"), names_sep = "_")

# Print the LaTeX table using xtable
xtable(summary_table_tidy, 
       caption = "Summary Statistics of the Regressors (Mean and SD)",
       label = "tab:summary-stat-stanford-rhc")

# 2. Treatment and time-variation
df_trt <- df_time_var %>% filter(trt == 1)
n_trt <- nrow(df_trt)
# Create the time-variation plot
time_variation_plot <- ggplot(df_trt, aes(x = tstart)) +
  geom_histogram(bins = 12, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Time of Heart Transplant",
       x = "Time of Heart Transplant",
       y = "Frequency") +
  theme_minimal()

ggsave(filename = "time_variation.png", plot = time_variation_plot, 
       path = "scripts/data-application/figures", width = 8, height = 6)


########
# Model fitting
########
### Impact of incorporating time-varying information on a marginal effect.
## Marginal effect
# Fit a model that ignores the time-varying nature of transplant (transplant as fixed)
fit_fixed <- coxph(Surv(futime, fustat) ~ age + surgery + year + trt, data = df_original, ties = "breslow")
summary(fit_fixed)

# Fit a model that includes time-varying nature (transplant as time-dependent)
fit_timevarying <- coxph(Surv(tstart, tstop, death) ~ age + surgery + year + trt, 
                         data = df_time_var, ties = "breslow")
summary(fit_timevarying)

# Compare model coefficients and likelihoods
print("Model ignoring time variation:")
print(summary(fit_fixed))

print("Model including time variation:")
print(summary(fit_timevarying))

# Extract the coefficients and statistics from the models
# For the model ignoring time-variation
coef_fixed <- summary(fit_fixed)$coef

# For the model including time-variation
coef_timevarying <- summary(fit_timevarying)$coef

# Create data frames with the desired columns
df_summary <- data.frame(
  Variable = rownames(coef_fixed),
  `Coef (SE) -- Non-time-varying` = paste0(round(coef_fixed[, 1], 3), " (", round(coef_fixed[, 3], 3), ")"),
  `P-value -- Non-time-varying` = round(coef_fixed[, 5], 4),
  `Coef (SE) -- Time-varying` = paste0(round(coef_timevarying[, 1], 3), " (", round(coef_timevarying[, 3], 3), ")"),
  `P-value -- Time-varying` = round(coef_timevarying[, 5], 4)
)

# Print the table in LaTeX format using xtable
print(xtable(df_summary, 
             caption = "Comparison of Cox Models: Fixed vs. Time-varying Transplant",
             label = "tab:impact-time-varying-marginal"), 
      include.rownames = FALSE)


#### 
## HTE
# Fit a model that ignores the time-varying nature of transplant (transplant as fixed)
fit_fixed <- coxph(Surv(futime, fustat) ~ age + surgery + year + trt*(1+age + surgery + year), 
                   data = df_original, ties = "breslow")
summary(fit_fixed)

# Fit a model that includes time-varying nature (transplant as time-dependent)
fit_timevarying <- coxph(Surv(tstart, tstop, death) ~ age + surgery + year + trt*(1+age + surgery + year), 
                         data = df_time_var, ties = "breslow")
summary(fit_timevarying)

# Compare model coefficients and likelihoods
print("Model ignoring time variation:")
print(summary(fit_fixed))

print("Model including time variation:")
print(summary(fit_timevarying))


# Extract the relevant coefficients from both models (trt to year:trt)
# For the model ignoring time-variation
coef_fixed <- summary(fit_fixed)$coef[c("trt", "age:trt", "surgery:trt", "year:trt"), ]

# For the model including time-variation
coef_timevarying <- summary(fit_timevarying)$coef[c("trt", "age:trt", "surgery:trt", "year:trt"), ]

# Create data frames with the desired columns
df_summary <- data.frame(
  Variable = rownames(coef_fixed),
  `Coef (SE) -- Non-time-varying` = paste0(round(coef_fixed[, 1], 3), " (", round(coef_fixed[, 3], 3), ")"),
  `P-value -- Non-time-varying` = round(coef_fixed[, 5], 4),
  `Coef (SE) -- Time-varying` = paste0(round(coef_timevarying[, 1], 3), " (", round(coef_timevarying[, 3], 3), ")"),
  `P-value -- Time-varying` = round(coef_timevarying[, 5], 4)
)

# Print the table in LaTeX format using xtable
print(xtable(df_summary, caption = "Comparison of Cox Models: Fixed vs. Time-varying (trt to year:trt)"), 
      include.rownames = FALSE)



### TV-CSL:
# Implement S-lasso. 
#   Let's obtain the HTE coefficients and use them as the outcome of interest
source("scripts/TV-CSL/time-varying-estimate.R")

# Regressors are age, surgery, year
# Rename regressor of df_time_var to X.1, X.2, X.3
# Standardizing X.1 (age) and X.3 (year) in df_time_var
df_time_var <- df_time_var %>%
  mutate(
    X.1 = scale(age),        # Standardize age
    X.2 = surgery,           # Keep surgery binary as is
    X.3 = scale(year),       # Standardize year
    Delta = death,
    W = trt
  )

# Standardizing X.1 (age) and X.3 (year) in df_original
df_original <- df_original %>%
  mutate(
    X.1 = scale(age),        # Standardize age
    X.2 = surgery,           # Keep surgery binary as is
    X.3 = scale(year)        # Standardize year
  )

source("scripts/TV-CSL/time-varying-estimate.R")
lasso_ret <-
  S_lasso(train_data = df_time_var,
          test_data = df_original,
          regressor_spec = "complex",
          HTE_spec = "linear",
          verbose = 2)
lasso_ret$beta_HTE
lasso_ret$HTE_est
# 1 min to get the 

# Compare the HTE estimate with time_varying, using standardized 
source("scripts/TV-CSL/time-varying-estimate.R")
cox_ret <-
  S_lasso(train_data = df_time_var,
          test_data = df_original,
          regressor_spec = "linear",
          HTE_spec = "linear",
          verbose = 2)
  # S_cox(train_data = df_time_var,
  #         test_data = df_original,
  #         regressor_spec = "linear",
  #         HTE_spec = "linear",
  #         verbose = 2)
cox_ret$beta_HTE
cox_ret$HTE_est

# # Calculate Mean Absolute Differencce (MAD)
# MAD <- mean(abs(lasso_ret$HTE_est - cox_ret$HTE_est))
# print(MAD)
# correlation <- cor(lasso_ret$HTE_est, cox_ret$HTE_est)
# print(correlation)
# 
# var(lasso_ret$HTE_est)
# var(cox_ret$HTE_est)

# library(ggplot2)
# 
# # Create a scatter plot of HTE estimates
# ggplot(data.frame(lasso_est = lasso_ret$HTE_est, cox_est = cox_ret$HTE_est), aes(x = lasso_est, y = cox_est)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = "red") +  # Line y=x for reference
#   labs(x = "Lasso HTE Estimate", y = "Cox HTE Estimate", title = "Comparison of HTE Estimates")

## Use TV-CSL: goal is to compare the coefficient estimate with S-lasso
source("scripts/TV-CSL/time-varying-estimate.R")
df_original <- df_original %>% 
  mutate(U_A = pmin(txtime,futime),
         Delta_A = txtime <= futime)
TV_CSL_ret <- TV_CSL(train_data = df_time_var, 
                     test_data = df_original, 
                     train_data_original = df_original, 
                     K = 2, 
                     prop_score_spec = "cox-linear-all-data", 
                     lasso_type = "S-lasso", 
                     regressor_spec = "complex",
                     HTE_spec = "linear",
                     final_model_method = "coxph",
                     id_var = "subject",
                     verbose = 1) 

TV_CSL_ret$beta_HTE_first_stages
TV_CSL_ret$beta_HTE
cox_ret$beta_HTE
lasso_ret$beta_HTE



TV_CSL_ret_no_warmstart <- TV_CSL(train_data = df_time_var, 
                     test_data = df_original, 
                     train_data_original = df_original, 
                     K = 2, 
                     prop_score_spec = "cox-linear-all-data", 
                     lasso_type = "S-lasso", 
                     regressor_spec = "complex",
                     HTE_spec = "linear",
                     final_model_method = "coxph",
                     id_var = "subject",
                     lasso_warmstart = F,
                     verbose = 1) 
TV_CSL_ret_no_warmstart$beta_HTEs
TV_CSL_ret$beta_HTEs
TV_CSL_ret$beta_HTE_first_stages




TV_CSL_ret_K_3 <- TV_CSL(train_data = df_time_var, 
                                  test_data = df_original, 
                                  train_data_original = df_original, 
                                  K = 3, 
                                  prop_score_spec = "cox-linear-all-data", 
                                  lasso_type = "S-lasso", 
                                  regressor_spec = "complex",
                                  HTE_spec = "linear",
                                  final_model_method = "coxph",
                                  id_var = "subject",
                                  lasso_warmstart = F,
                                  verbose = 1) 
TV_CSL_ret_K_3$beta_HTEs
TV_CSL_ret$beta_HTEs


source("scripts/TV-CSL/time-varying-estimate.R")
TV_CSL_ret_linear_eta <- TV_CSL(train_data = df_time_var, 
                     test_data = df_original, 
                     train_data_original = df_original, 
                     K = 2, 
                     prop_score_spec = "cox-linear-all-data", 
                     lasso_type = "S-lasso", 
                     regressor_spec = "linear",
                     HTE_spec = "linear",
                     final_model_method = "coxph",
                     id_var = "subject",
                     lasso_warmstart = F,
                     verbose = 1) 

TV_CSL_ret_linear_eta$beta_HTE
cox_ret$beta_HTE

