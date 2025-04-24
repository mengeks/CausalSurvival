########
# Load and prepare the data
########
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
df_bwh <- 
  readRDS("data/bwh-heart-transplant/cleaned_bwh_transplant_data.rds")

########
# Summary statistics
########

# \xmeng{ also ahve a column on the number of transpalnts and non-transplants }
df_summary <- df_bwh %>%
  select(age, bmi, blood_type, ventilation, support_device, transplant) %>%
  group_by(transplant) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  pivot_longer(cols = -transplant, names_to = c("Variable", ".value"), names_sep = "_")

print(xtable(df_summary, caption = "Summary statistics by transplant status"), include.rownames = FALSE)

########
# Time-Varying Treatment Setup
########
# \xmeng{ we do not need to rename it in the same way of }
df_bwh_timevar <- df_bwh %>%
  # filter(transplant == TRUE) %>%
  mutate(subject = row_number(),
         tstart = 0,
         tstop = time_to_event,
         death = event,
         trt = as.integer(tstop >= time_to_tx),
         futime = time_to_event,
         fustat = event,
         txtime = time_to_tx,
         surgery = as.integer(support_device),
         year = as.numeric(format(listing_date, "%Y")))

########
# Comparing Fixed Treatment Cox
########
### Model: Fixed Treatment Cox
df_bwh_fixed <- df_bwh_timevar %>%
  mutate(age = scale(age),
         year = scale(year))

fit_fixed <- 
  coxph(Surv(futime, fustat) ~ age + surgery + year + trt, 
        data = df_bwh_fixed, 
        ties = "breslow")
summary(fit_fixed)
# \xmeng{ it's not working: all results are NA }
# Inspection: Inspected all regressors by taking table of the 
# binary variables and histogram of the continuous variables
# Found two things: 1. fustat is zero for all 881 patients (the most likely reason)
table(df_bwh_fixed$fustat)
# 2. trt is 1 for 878 out of 881 patients (the less likely reason)
table(df_bwh_fixed$trt)

# First attempt: remove filter(transplant == TRUE) in construction of df_bwh_timevar
# it did not work because it indeed get some ones 
# fustat now has 1148 zeros out of 1221 rows

## How to make the model work?
## Answer: when we remove filter(transplant == TRUE) AND we remove the trt variable
## Notice that after removing the filter, the trt variable has 340 NA entries
sum(is.na(df_bwh_fixed$age)) # == 340

## but even we remove the trt variable, if the filter is not removed, then it still will fail
## so the primary criterion is that we need to make the fustat variable contains enought 0 and 1.
## and the secondary criterion is that we need to have non NA treatment variable
## so what values 

# So there is an issue that some rows of data has NA in trt -- is this expected?
# I feel a treatment variable could be:
# - 1 (obtained transplant)
# - 0 (confirmed no transplant, i.e., died)
# - NA (lost of follow up (including recovered right?) of the patient, 
#   and no transplant during the period of following up
#   because if we follow up means we know the death)




### Model: Time-Varying Treatment Cox
fit_timevarying <- 
  coxph(Surv(tstart, tstop, death) ~ scale(age) + surgery + scale(year) + trt,
        data = df_bwh_timevar, ties = "breslow")
summary(fit_timevarying)
# \xmeng{ it's not working: all results are NA }

### 📊 Compare Coefficients Between Models

coef_fixed <- summary(fit_fixed)$coef
coef_timevar <- summary(fit_timevarying)$coef

compare_table <- data.frame(
  Variable = rownames(coef_fixed),
  `Coef (SE) -- Fixed` = paste0(round(coef_fixed[,1], 3), " (", round(coef_fixed[,3], 3), ")"),
  `P-value -- Fixed` = round(coef_fixed[,5], 4),
  `Coef (SE) -- Time-varying` = paste0(round(coef_timevar[,1], 3), " (", round(coef_timevar[,3], 3), ")"),
  `P-value -- Time-varying` = round(coef_timevar[,5], 4)
)

print(xtable(compare_table, caption = "Comparison of Cox Models: Fixed vs. Time-varying Transplant"),
      include.rownames = FALSE)


########
# 📊 Visualization: Timing of Treatment
########
ggplot(df_bwh_timevar %>% filter(trt == 1), aes(x = time_to_tx)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Heart Transplant Timing", x = "Months from Listing to Transplant", y = "Count") +
  theme_minimal()
