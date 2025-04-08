library(ggplot2)
library(dplyr)

df_time_to_treatment <- readRDS("data/tadpull/df_time_to_treatment.rds")
df_customer_summary <- readRDS("data/tadpull/df_customer_summary.rds")

df_original <- df_time_to_treatment %>% 
  mutate(U_A = pmin(tx_time,outcome_time),
         Delta_A = tx_time < outcome_time)
table(df_original$Delta_A)
# k1 is 0.1
# FALSE  TRUE 
# 687   129
# k1 is 0.5
# FALSE  TRUE 
# 545    47
# k1 is 0.25
# FALSE  TRUE 
# 526    57

# focus on df_customer_summary, 
df_time_to_treatment <- df_time_to_treatment %>%
  filter(tx_time > 0) # 841 users

# Histogram of treatment time
ggplot(df_original%>% filter(!is.na(tx_time), Delta_A==T),
  # df_time_to_treatment %>% filter(!is.na(tx_time)), 
       aes(x = tx_time)) +
  geom_histogram(binwidth = 7, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Treatment",
    subtitle = "Based on first page view",
    x = "Treatment Time (days)",
    y = "Number of Customers"
  ) +
  theme_minimal()

# Histogram of outcome time
ggplot(df_time_to_treatment %>% filter(!is.na(outcome_time)), 
       aes(x = outcome_time)) +
  geom_histogram(binwidth = 7, fill = "firebrick", color = "white", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Outcome",
    subtitle = "Based on first page view",
    x = "Outcome Time (days)",
    y = "Number of Customers"
  ) +
  theme_minimal()

# Bar chart of treatment time categories
ggplot(df_time_to_treatment %>% filter(tx_category != "No treatment"), 
       aes(x = tx_category)) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Treatment",
    subtitle = "Based on first page view",
    x = "Treatment Time",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  # Order the categories logically
  scale_x_discrete(limits = c("Before interaction", "Same day", "Within a week", 
                              "Within a month", "1-3 months", "3-6 months", "Over 6 months"))

# Bar chart of outcome time categories
ggplot(df_time_to_treatment %>% filter(outcome_category != "No outcome"), 
       aes(x = outcome_category)) +
  geom_bar(fill = "firebrick", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Outcome",
    subtitle = "Based on first page view",
    x = "Outcome Time",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  # Order the categories logically
  scale_x_discrete(limits = c("Before interaction", "Same day", "Within a week", 
                              "Within a month", "1-3 months", "3-6 months", "Over 6 months"))

# Comparison of timelines
df_time_comparison <- df_customer_summary %>%
  filter(!is.na(first_interaction_date)) %>%
  mutate(
    # Renamed variables
    interaction_to_tx = as.numeric(difftime(first_treatment_date, 
                                            first_interaction_date,
                                            units = "days")),
    transaction_to_tx = as.numeric(difftime(first_treatment_date, 
                                            first_transaction_date,
                                            units = "days")),
    interaction_to_transaction = as.numeric(difftime(first_transaction_date, 
                                                     first_interaction_date,
                                                     units = "days")),
    # Add outcome timelines
    interaction_to_outcome = as.numeric(difftime(first_outcome_date, 
                                                 first_interaction_date,
                                                 units = "days")),
    tx_to_outcome = as.numeric(difftime(first_outcome_date, 
                                        first_treatment_date,
                                        units = "days"))
  )

# Summary statistics
time_summary <- df_time_comparison %>%
  summarize(
    total_customers = n(),
    treated_customers = sum(!is.na(interaction_to_tx)),
    outcome_customers = sum(!is.na(interaction_to_outcome)),
    
    # Treatment timing
    avg_interaction_to_tx = mean(interaction_to_tx, na.rm = TRUE),
    median_interaction_to_tx = median(interaction_to_tx, na.rm = TRUE),
    avg_transaction_to_tx = mean(transaction_to_tx, na.rm = TRUE),
    median_transaction_to_tx = median(transaction_to_tx, na.rm = TRUE),
    
    # Interaction to transaction
    avg_interaction_to_transaction = mean(interaction_to_transaction, na.rm = TRUE),
    median_interaction_to_transaction = median(interaction_to_transaction, na.rm = TRUE),
    
    # Outcome timing
    avg_interaction_to_outcome = mean(interaction_to_outcome, na.rm = TRUE),
    median_interaction_to_outcome = median(interaction_to_outcome, na.rm = TRUE),
    avg_tx_to_outcome = mean(tx_to_outcome, na.rm = TRUE),
    median_tx_to_outcome = median(tx_to_outcome, na.rm = TRUE)
  )

print(time_summary)

# Scatterplot comparing treatment and outcome times
ggplot(df_time_comparison %>% filter(!is.na(interaction_to_tx), !is.na(interaction_to_outcome)), 
       aes(x = interaction_to_tx, y = interaction_to_outcome)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Treatment vs Outcome Timing",
    x = "Days from First Interaction to Treatment",
    y = "Days from First Interaction to Outcome",
    caption = "Points above the line indicate outcome occurred after treatment"
  ) +
  theme_minimal() +
  coord_equal()

# Additional visualization: time from treatment to outcome
ggplot(df_time_comparison %>% filter(!is.na(tx_to_outcome)), 
       aes(x = tx_to_outcome)) +
  geom_histogram(binwidth = 7, fill = "darkgreen", color = "white", alpha = 0.7) +
  labs(
    title = "Time from Treatment to Outcome",
    x = "Days from Treatment to Outcome",
    y = "Number of Customers"
  ) +
  theme_minimal()

# Next: we should use the other outcome in the data.
#   -- it's reasonable that the outcome happens before the treatment, i.e., the outcoe date should always be greater  -- done
# Next: fit the model -- Start by trying the survival model -- we need to separate into start and end 
# What do we need to fit the model -- we need to create the se
 
df_original <- df_time_to_treatment %>% 
  mutate(U_A = pmin(tx_time,outcome_time),
         Delta_A = tx_time < outcome_time)
table(df_original$Delta_A)
# k1 is 0.5
# FALSE  TRUE 
# 545    47
# k1 is 0.25
# FALSE  TRUE 
# 526    57
# meaning: 526 did not get the treatment 
# 57 got the treatment
# the rest: did not purchase anything of value > k2 (but they did purchase things > k1), i.e., is censored

source("scripts/TV-CSL/time-varying-estimate.R")
# align data format as required 
df_original <- df_original %>% 
  mutate(id = 1:n(),
         U =
           ifelse(is.na(outcome_time), max(outcome_time, na.rm = T), outcome_time),  # if the outcome_time is NA then set it to the largest outcome_time, otherwise set it to outcome_time
         Delta =ifelse(is.na(outcome_time), 0, 1),
         A = tx_time)

fit_timefixed <- coxph(
  Surv(U, Delta) ~ 1 + Delta_A, 
  data = df_original, 
  ties = "breslow")
summary(fit_timefixed)


df_processed <- preprocess_data(single_data = df_original, 
                                run_time_varying = T)


fit_timevarying <- coxph(
  Surv(tstart, tstop, Delta) ~ 1 + W, 
  data = df_processed, ties = "breslow")
summary(fit_timevarying)


coef(fit_timefixed)
confint(fit_timefixed)

coef(fit_timevarying)
confint(fit_timevarying) 
# more humble confidence intervals

# Next: reflect on choosing k1 and k2
# Next: write up the analysis to get clarity
# Next: implement the S-LASSO
# Next: add control variables
