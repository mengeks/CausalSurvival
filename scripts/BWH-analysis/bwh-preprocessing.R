library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(text)

df_raw <- read_xlsx("data/bwh-heart-transplant/bwh-listing.xlsx")

df <- df_raw %>%
  mutate(listing_date = as.Date(`Listing Date`)) %>%
  mutate(age = as.numeric(`Age in Years at Time of Listing`)) %>%
  mutate(transplant_date = as.Date(`Transplant Date`)) %>%
  mutate(death_date = as.Date(`Date Died, for Patient Removed from the Waiting List for Reason of Death`)) %>%
  mutate(censor_date = as.Date(`Removal Date of Registration`)) %>%
  mutate(transplant = !is.na(transplant_date)) %>%
  mutate(
    time_to_event = as.numeric(difftime(
      coalesce(death_date, censor_date),
      listing_date,
      units = "days"
    )) / 30.44
  ) %>%
  mutate(
    event = ifelse(!is.na(death_date), 1, 0),
    time_to_tx = as.numeric(difftime(
      transplant_date,
      listing_date,
      units = "days"
    )) / 30.44
  ) %>%
  mutate(
    gender = str_to_lower(`Birth Sex`),
    race = str_to_lower(`Ethnicity Category`),
    blood_type = str_to_upper(`Blood Type`)
  ) %>%
  mutate(
    diagnosis = str_to_lower(`Primary Diagnosis at Time of Listing`),
    status_listing = str_to_lower(`Wait List Medical Urgency Status at Transplant Offer/Removal Current Time`)
  ) %>%
  mutate(
    height_cm = as.numeric(`Height (CM) at Listing`),
    weight_kg = as.numeric(`Weight (kg) at Listing`),
    bmi = as.numeric(`BMI at Listing`)
  ) %>%
  mutate(
    ventilation = str_detect(`Life Support Type Ventilator at Listing`, regex("yes|mechanical|intubated", ignore_case = TRUE)),
    ecmo = str_detect(`Life Support Type ECMO at Listing`, regex("yes", ignore_case = TRUE)),
    iabp = str_detect(`Life Support Type IABP at Listing`, regex("yes", ignore_case = TRUE)),
    inotropes = str_detect(`Life Support Type IV Inotropes at Listing`, regex("yes", ignore_case = TRUE)),
    support_device = ecmo | iabp | inotropes
  ) %>%
  select(
    listing_date,
    transplant_date,
    death_date,
    censor_date,
    transplant,
    time_to_event,
    event,
    time_to_tx,
    age,
    gender,
    race,
    blood_type,
    status_listing,
    diagnosis,
    height_cm,
    weight_kg,
    bmi,
    ventilation,
    ecmo,
    iabp,
    inotropes,
    support_device
  ) %>%
  filter(!is.na(listing_date))

# save the data as rds
saveRDS(df, "data/bwh-heart-transplant/cleaned_bwh_transplant_data.rds")


# EDA
# I have heard that O type blood has longer waiting time. Make a EDA and an initial modelling
library(tidyverse)
library(ggpubr)

# Load cleaned data
df <- readRDS("data/bwh-heart-transplant/cleaned_bwh_transplant_data.rds")

# Filter only patients who were transplanted (i.e., time_to_tx is observed)
df_tx <- df %>% filter(transplant == TRUE, !is.na(time_to_tx), time_to_tx > 0)

# Summary statistics of waiting time by blood type
df_tx %>%
  group_by(blood_type) %>%
  summarise(
    n = n(),
    median_wait = median(time_to_tx, na.rm = TRUE),
    mean_wait = mean(time_to_tx, na.rm = TRUE),
    sd_wait = sd(time_to_tx, na.rm = TRUE),
    q25 = quantile(time_to_tx, 0.25, na.rm = TRUE),
    q75 = quantile(time_to_tx, 0.75, na.rm = TRUE)
  ) %>%
  arrange(desc(median_wait))

# Boxplot: Waiting time by blood type
ggplot(df_tx, aes(x = blood_type, y = time_to_tx, fill = blood_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +
  labs(
    title = "Waiting Time (Months) by Blood Type",
    x = "Blood Type",
    y = "Time to Transplant (months)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Violin plot alternative (optional)
ggplot(df_tx, aes(x = blood_type, y = time_to_tx, fill = blood_type)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Distribution of Waiting Time by Blood Type",
    x = "Blood Type",
    y = "Time to Transplant (months)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Kruskal-Wallis test (non-parametric test for differences)
kruskal.test(time_to_tx ~ blood_type, data = df_tx)

# Optional: pairwise comparisons (if Kruskal-Wallis is significant)
pairwise.wilcox.test(df_tx$time_to_tx, df_tx$blood_type, p.adjust.method = "BH")
