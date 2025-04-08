library(dplyr)

df_page_view <- read.csv("data/tadpull/ts_pixel_page_view_data.csv") %>% 
  select(-X) # remove the first column
df_sales <- read.csv("data/tadpull/ts_sales_lines_data.csv") %>%
  select(-X) # remove the first column

# Get summary of purchases per user
n_purchase_by_user <- df_sales %>% 
  group_by(customer_id) %>%
  summarise(n_purchase = n())
summary(n_purchase_by_user$n_purchase)

# Set k1 and k2 thresholds
# Try differnet threshold to get more trts
summary(df_sales$gross_amount)
k1 = quantile(df_sales$gross_amount, probs = 0.1)
k2 = quantile(df_sales$gross_amount, probs = 0.75)



# NEW VARIABLE DEFINITIONS:
# Treatment: first time a person's purchase amount is greater than k1
# Outcome: the first time a person's purchase amount is greater than k2 (previously "other variable")
# Post_treatment_outcome: after the treatment date, the first time a person's purchase amount is greater than k2 (previously "outcome")

# Clean up the date format of df_page_view timestamps
df_page_view_clean <- df_page_view %>%
  # Convert timestamps to POSIXct format
  mutate(
    page_view_start_tstamp = as.POSIXct(page_view_start_tstamp),
    page_view_end_tstamp = as.POSIXct(page_view_end_tstamp)
  )

# Calculate first interaction date for each customer from page views
df_first_interaction <- df_page_view_clean %>%
  group_by(customer_id) %>%
  summarize(
    first_interaction_date = min(page_view_start_tstamp, na.rm = TRUE)
  )

# Clean sales data
df_sales_clean <- df_sales %>%
  # Convert transaction_date to POSIXct format
  mutate(transaction_date = as.POSIXct(transaction_date))

# Join sales data with first interaction dates
df_sales_with_interaction <- df_sales_clean %>%
  # Left join to keep all sales records
  left_join(df_first_interaction, by = "customer_id")

# Calculate first treatment date for each customer (unchanged)
df_treatments <- df_sales_with_interaction %>%
  arrange(customer_id, transaction_date) %>%
  group_by(customer_id) %>%
  mutate(
    trt = ifelse(gross_amount > k1, 1, 0),
    cumulative_trt = cumsum(trt),
    is_first_trt = (cumulative_trt == 1 & trt == 1)
  )

# Identify first treatment date
df_first_trt_dates <- df_treatments %>%
  filter(is_first_trt) %>%
  group_by(customer_id) %>%
  summarize(first_trt_date = min(transaction_date, na.rm = TRUE))

# Join treatment dates back to main dataset
df_with_trt_dates <- df_treatments %>%
  left_join(df_first_trt_dates, by = "customer_id")

# Process OUTCOME (previously "other variable"): first time purchase > k2
df_outcomes <- df_with_trt_dates %>%
  group_by(customer_id) %>%
  arrange(customer_id, transaction_date) %>%
  mutate(
    outcome = ifelse(gross_amount > k2, 1, 0),
    cumulative_outcome = cumsum(outcome),
    is_first_outcome = (cumulative_outcome == 1 & outcome == 1)
  )

# Get first outcome dates
df_first_outcome_dates <- df_outcomes %>%
  filter(is_first_outcome) %>%
  group_by(customer_id) %>%
  summarize(
    first_outcome_date = min(transaction_date, na.rm = TRUE),
    first_outcome_value = first(gross_amount)
  )

# Join outcome dates
df_with_outcomes <- df_outcomes %>%
  left_join(df_first_outcome_dates, by = "customer_id")

# Process POST_TREATMENT_OUTCOME (previously "outcome"): first time purchase > k2 AFTER treatment
df_complete <- df_with_outcomes %>%
  group_by(customer_id) %>%
  mutate(
    post_treatment_outcome = case_when(
      is.na(first_trt_date) ~ 0,
      gross_amount > k2 & transaction_date > first_trt_date ~ 1,
      TRUE ~ 0
    ),
    cumulative_post_treatment_outcome = cumsum(post_treatment_outcome),
    is_first_post_treatment_outcome = (cumulative_post_treatment_outcome == 1 & post_treatment_outcome == 1)
  )

# Get first post_treatment_outcome dates
df_first_post_treatment_outcome_dates <- df_complete %>%
  filter(is_first_post_treatment_outcome) %>%
  group_by(customer_id) %>%
  summarize(
    first_post_treatment_outcome_date = min(transaction_date, na.rm = TRUE),
    first_post_treatment_outcome_value = first(gross_amount)
  )

# Complete the final dataset
df_final <- df_complete %>%
  left_join(df_first_post_treatment_outcome_dates, by = "customer_id")

# Final dataset now has:
# - Treatment info (first purchase > k1)
# - Outcome info (first purchase > k2)
# - Post_treatment_outcome info (first purchase > k2 after treatment date)


# Now I just want to get one row per customer_id
#   where i need to know the first treamtent date, treatment status, first outcome date, outcome status
# first treamtent date could be NA, meaning no treatment, 
# first outcome date could be NA, meaning no outcome, i.e., outcome is censored.
# Create customer summary using first_interaction_date instead of first_transaction_date
df_customer_summary <- df_final %>%
  group_by(customer_id) %>%
  summarize(
    # First interaction (from page views)
    first_interaction_date = first(first_interaction_date),
    
    # First transaction (from sales)
    first_transaction_date = min(transaction_date, na.rm = TRUE),
    
    # Treatment information
    treatment_status = max(trt, na.rm = TRUE),
    first_treatment_date = first(first_trt_date),
    
    # Outcome information
    outcome_status = max(outcome, na.rm = TRUE),
    first_outcome_date = first(first_outcome_date)
  )



# df_customer_summary looks good
# Now we want to plot the histogram of the treatment dates
# first we need to calculate the time between the first transaction date and the first treatment date
# Calculate time between first transaction and first treatment

# What is the encoding of treatment time when there is no treatment
# Answer: NA

# Calculate time between first interaction and first treatment/outcome
df_time_to_treatment <- df_customer_summary %>%
  # Include all customers who have page view data
  filter(!is.na(first_interaction_date)) %>%
  # Calculate time difference in days
  mutate(
    tx_time = as.numeric(difftime(first_treatment_date, 
                                  first_interaction_date,
                                  units = "days")),
    outcome_time = as.numeric(difftime(first_outcome_date, 
                                       first_interaction_date,
                                       units = "days")),
    # Add separate categories for treatment and outcome times
    tx_category = case_when(
      is.na(tx_time) ~ "No treatment",
      tx_time < 0 ~ "Before interaction",
      tx_time == 0 ~ "Same day",
      tx_time <= 7 ~ "Within a week",
      tx_time <= 30 ~ "Within a month",
      tx_time <= 90 ~ "1-3 months",
      tx_time <= 180 ~ "3-6 months",
      TRUE ~ "Over 6 months"
    ),
    outcome_category = case_when(
      is.na(outcome_time) ~ "No outcome",
      outcome_time < 0 ~ "Before interaction",
      outcome_time == 0 ~ "Same day",
      outcome_time <= 7 ~ "Within a week",
      outcome_time <= 30 ~ "Within a month",
      outcome_time <= 90 ~ "1-3 months",
      outcome_time <= 180 ~ "3-6 months",
      TRUE ~ "Over 6 months"
    )
  )

# save data 
saveRDS(df_customer_summary, "data/tadpull/df_customer_summary.rds")
saveRDS(df_time_to_treatment, "data/tadpull/df_time_to_treatment.rds")
