# Load necessary libraries
library(dplyr)
library(readr)

# # Read the CSV file
# data <- read_csv("~/Downloads/eta-type-linear_HTE-type-linear_beta-HTE.csv")
# # Filter rows where X.3:W is not NA
# filtered_data <- data %>% filter(!is.na(`X.3:W`))

data <- read_csv("~/Downloads/eta-type-complex_HTE-type-linear_beta-HTE.csv")
filtered_data <- data[!grepl(",", data$V10), ]

names(filtered_data)[names(filtered_data) %in% c("V7", "V8", "V9", "V10")] <- c("W", "X.1:W", "X.2:W", "X.3:W")

# Convert the renamed columns to numeric
filtered_data$W <- as.numeric(filtered_data$W)
filtered_data$`X.1:W` <- as.numeric(filtered_data$`X.1:W`)
filtered_data$`X.2:W` <- as.numeric(filtered_data$`X.2:W`)
filtered_data$`X.3:W` <- as.numeric(filtered_data$`X.3:W`)

# Compute the average for each combination of iteration, lasso_type, eta_type, HTE_type, and stage
averaged_data <- filtered_data %>%
  group_by(iteration, lasso_type, eta_type, HTE_type, stage) %>%
  summarise(
    avg_W = mean(`W`, na.rm = TRUE),
    avg_X1_W = mean(`X.1:W`, na.rm = TRUE),
    avg_X2_W = mean(`X.2:W`, na.rm = TRUE),
    avg_X3_W = mean(`X.3:W`, na.rm = TRUE)
  ) %>%
  ungroup()

averaged_data <- averaged_data %>%
  mutate(euclidean_distance = sqrt((avg_W - 0)^2 + (avg_X1_W - 1)^2 + (avg_X2_W - 1)^2 + (avg_X3_W - 1)^2))

# Print or save the result
print(averaged_data)

averaged_distance_data <- averaged_data %>%
  group_by(lasso_type, eta_type, HTE_type, stage) %>%
  summarise(avg_euclidean_distance = mean(euclidean_distance, na.rm = TRUE)) %>%
  ungroup()

# Print or save the result with averaged Euclidean distances
print(averaged_distance_data)
write_csv(averaged_distance_data, "~/Downloads/averaged_euclidean_distance.csv")



# Step 1: Read the CSV file
file_path <- "~/Downloads/eta-type-complex_HTE-type-linear_MSE.csv"
df <- read.csv(file_path, header = TRUE)

# Step 2: Average MSE over 'k' for each unique combination of iteration, eta_type, HTE_type, lasso_type, and stage
df_avg_k <- df %>%
  group_by(iteration, eta_type, HTE_type, lasso_type, stage) %>%
  summarise(MSE_avg_k = mean(MSE, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Average the previously averaged MSE over 'iteration' for each unique combination of eta_type, HTE_type, lasso_type, and stage
final_df <- df_avg_k %>%
  group_by(eta_type, HTE_type, lasso_type, stage) %>%
  summarise(MSE_final_avg = mean(MSE_avg_k, na.rm = TRUE)) %>%
  ungroup()

# View the final result
print(final_df)



# load a dataset 
