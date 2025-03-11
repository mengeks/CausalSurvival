library(dplyr)
# q: Seems that we are automatically at the project folder -- when do we need here package? where does here::here() specify?
# a: here::here() specifies the project folder. We need to use package here when we want to use the function here().
df_page_view <- read.csv("data/data_share_tadpull/ts_pixel_page_view_data.csv") %>% 
  select(-X) # remove the first column
df_sales <- read.csv("data/data_share_tadpull/ts_sales_lines_data.csv") %>%
  select(-X) # remove the first column


# Next: go to iav meeting notes to see what is the business question that we were talking about
#   goal is to find the variable and find the cutoff
tmp <- df_sales %>% filter(customer_id=="23053465")

summary(df_sales$gross_amount)
hist(df_sales$gross_amount)


n_purchase_by_user <- df_sales %>% 
  group_by(customer_id) %>%
  summarise(n_purchase = n())
summary(n_purchase_by_user$n_purchase)
# a lot of them purchased more than once so it's okay..

# set k1: threshold for, k2
# initial guess: k1 = median / Q1, k2
summary(df_sales$gross_amount)
k1 = quantile(df_sales$gross_amount, probs = 0.25)
k2 = quantile(df_sales$gross_amount, probs = 0.75)
# Treatment: first time a person's purchase amount is greater than k1
# Outcome: after the treatment date, the first time a person's purchase amount is greater than k2
# Other variable of interest: the first time a person's purchase amount is greater than k2
# EDA: within each user, first order in ascending order of dates
# then compute Treatment, Outcome, and Other variable of interest
# First, clean up the date format of df_page_view timestamps
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

# Now we can use df_sales_with_interaction as the base for our treatment-outcome analysis
# Calculate first treatment date for each customer
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

# Continue with outcome processing similar to before
df_outcomes <- df_with_trt_dates %>%
  group_by(customer_id) %>%
  arrange(customer_id, transaction_date) %>%
  mutate(
    outcome = case_when(
      is.na(first_trt_date) ~ 0,
      gross_amount > k2 & transaction_date > first_trt_date ~ 1,
      TRUE ~ 0
    ),
    cumulative_outcome = cumsum(outcome),
    is_first_outcome = (cumulative_outcome == 1 & outcome == 1)
  )

# Get first outcome dates
df_first_outcome_dates <- df_outcomes %>%
  filter(is_first_outcome) %>%
  group_by(customer_id) %>%
  summarize(first_outcome_date = min(transaction_date, na.rm = TRUE))

# Join outcome dates
df_with_outcomes <- df_outcomes %>%
  left_join(df_first_outcome_dates, by = "customer_id")

# Process other variables
df_trt_outcome <- df_with_outcomes %>%
  group_by(customer_id) %>%
  mutate(
    other_var = ifelse(gross_amount > k2, gross_amount, NA),
    has_other_var = !is.na(other_var),
    cumulative_other_var = cumsum(has_other_var),
    is_first_other_var = (cumulative_other_var == 1 & has_other_var)
  )

# Get first other_var dates
df_first_other_var_dates <- df_trt_outcome %>%
  filter(is_first_other_var) %>%
  group_by(customer_id) %>%
  summarize(
    first_other_var_date = min(transaction_date, na.rm = TRUE),
    first_other_var_value = first(other_var)
  )

# Complete the dataset
df_full_trt_outcome <- df_trt_outcome %>%
  left_join(df_first_other_var_dates, by = "customer_id")

# Create customer summary using first_interaction_date instead of first_transaction_date
df_customer_summary <- df_full_trt_outcome %>%
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
    first_outcome_date = first(first_outcome_date),
    
    # Other variable information
    other_var_status = max(has_other_var, na.rm = TRUE),
    first_other_var_value = first(first_other_var_value),
    first_other_var_date = first(first_other_var_date)
  )

# Next: i just want to get one row per customer_id
#   where i need to know the first treamtent date, treatment status, first outcome date, outcome status
# first treamtent date could be NA, meaning no treatment, 
# first outcome date could be NA, meaning no outcome, i.e., outcome is censored.


# df_customer_summary looks good
# Now we want to plot the histogram of the treatment dates
# first we need to calculate the time between the first transaction date and the first treatment date
# Calculate time between first transaction and first treatment


# Calculate time between first interaction and first treatment
df_time_to_treatment <- df_customer_summary %>%
  # Only include customers who received treatment and have page view data
  filter(treatment_status == 1, !is.na(first_interaction_date)) %>%
  # Calculate time difference in days
  mutate(
    days_to_treatment = as.numeric(difftime(first_treatment_date, 
                                            first_interaction_date,
                                            units = "days")),
    # Add a category for plotting
    time_category = case_when(
      days_to_treatment == 0 ~ "Same day",
      days_to_treatment <= 7 ~ "Within a week",
      days_to_treatment <= 30 ~ "Within a month",
      days_to_treatment <= 90 ~ "1-3 months",
      days_to_treatment <= 180 ~ "3-6 months",
      TRUE ~ "Over 6 months"
    )
  )

# Basic histogram of days to treatment
# since the view 
ggplot(df_time_to_treatment, aes(x = days_to_treatment)) +
  geom_histogram(binwidth = 7, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Treatment",
    subtitle = "Based on first page view rather than first transaction",
    x = "Days to Treatment",
    y = "Number of Customers"
  ) +
  theme_minimal()

# Bar chart of time categories
ggplot(df_time_to_treatment, aes(x = time_category)) +
  geom_bar(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Time from First Interaction to First Treatment",
    subtitle = "Based on first page view rather than first transaction",
    x = "Time to Treatment",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  # Order the categories logically
  scale_x_discrete(limits = c("Same day", "Within a week", "Within a month", 
                              "1-3 months", "3-6 months", "Over 6 months"))

# Add a comparison between first interaction and first transaction
df_time_comparison <- df_customer_summary %>%
  filter(treatment_status == 1, !is.na(first_interaction_date)) %>%
  mutate(
    days_interaction_to_treatment = as.numeric(difftime(first_treatment_date, 
                                                        first_interaction_date,
                                                        units = "days")),
    days_transaction_to_treatment = as.numeric(difftime(first_treatment_date, 
                                                        first_transaction_date,
                                                        units = "days")),
    days_interaction_to_transaction = as.numeric(difftime(first_transaction_date, 
                                                          first_interaction_date,
                                                          units = "days"))
  )

# Summary statistics
time_summary <- df_time_comparison %>%
  summarize(
    total_treated_customers = n(),
    avg_days_interaction_to_treatment = mean(days_interaction_to_treatment),
    median_days_interaction_to_treatment = median(days_interaction_to_treatment),
    avg_days_transaction_to_treatment = mean(days_transaction_to_treatment),
    median_days_transaction_to_treatment = median(days_transaction_to_treatment),
    avg_days_interaction_to_transaction = mean(days_interaction_to_transaction),
    median_days_interaction_to_transaction = median(days_interaction_to_transaction)
  )

print(time_summary)

# Scatterplot comparing the two time measures
ggplot(df_time_comparison, aes(x = days_interaction_to_treatment, 
                               y = days_transaction_to_treatment)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Comparison of Treatment Timing Metrics",
    x = "Days from First Interaction to Treatment",
    y = "Days from First Transaction to Treatment",
    caption = "Points below the line indicate first transaction occurred after first interaction"
  ) +
  theme_minimal() +
  coord_equal()
# Next: read the plots
# Next: get number of customers whose treatment date > 0
nrow(df_time_to_treatment %>% filter(days_to_treatment > 0)) # 841 users
# Next: add control variables
# Next: fit the model