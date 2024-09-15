## Note: right now it has error
# Error: '\_' is an unrecognized escape in character string (<input>:13:115)

library(dplyr)
library(tidyr)
library(here)

# Define the path to the CSV file for time metrics
file_path_time <- here::here("tables/continuous-R-learner", "aggregated_time_metrics.csv")

# Read the time data
time_metrics_df <- read.csv(file_path_time)

# Format the Avg_Time values to 3 significant digits
time_metrics_df <- time_metrics_df %>%
  mutate(Avg_Time = signif(Avg_Time, 3))

# Function to generate a custom LaTeX table for a given R value
generate_custom_time_latex <- function(R_value) {
  
  # Filter data for the chosen R value
  filtered_df <- time_metrics_df %>%
    filter(R == R_value) %>%
    arrange(n)
  
  # Create the LaTeX table manually
  latex_table <- "\\begin{table}[ht]\n\\centering\n"
  latex_table <- paste0(latex_table, "\\caption{Average time metrics for R = ", R_value, "}\n")
  latex_table <- paste0(latex_table, "\\begin{tabular}{l|l|cc}\n")
  latex_table <- paste0(latex_table, "  \\toprule\n")
  latex_table <- paste0(latex_table, "   \\multirow{2}{*}{n} & \\multirow{2}{*}{Method} & \\multicolumn{2}{c}{Avg\_Time} \\\\\n")
  latex_table <- paste0(latex_table, "    &  & linear-interaction & log \\\\\n")
  latex_table <- paste0(latex_table, "  \\midrule\n")
  
  # Loop over the different sample sizes n
  for (n_value in unique(filtered_df$n)) {
    n_data <- filtered_df %>% filter(n == n_value)
    
    # Add multirow for 'n' and the values for R-Lasso
    r_lasso <- n_data %>% filter(Method == "R-Lasso")
    s_lasso <- n_data %>% filter(Method == "S-Lasso")
    
    latex_table <- paste0(latex_table, "  \\multirow{2}{*}{", n_value, "} & R-Lasso & ", 
                          r_lasso$Avg_Time[1], " & ", r_lasso$Avg_Time[2], " \\\\\n")
    
    # Add the values for S-Lasso
    latex_table <- paste0(latex_table, "   & S-Lasso & ", 
                          s_lasso$Avg_Time[1], " & ", s_lasso$Avg_Time[2], " \\\\\n")
    
    latex_table <- paste0(latex_table, "  \\midrule\n")
  }
  
  # Close the table
  latex_table <- paste0(latex_table, "  \\bottomrule\n\\end{tabular}\n\\end{table}\n")
  
  return(latex_table)
}

# Generate LaTeX table for R = 200 and save it as a text file
latex_table_200 <- generate_custom_time_latex(R_value = 200)
writeLines(latex_table_200, con = here::here("tables/continuous-R-learner", "average_time_metrics_R_200.txt"))

# Generate LaTeX table for R = 500 and save it as a text file
latex_table_500 <- generate_custom_time_latex(R_value = 500)
writeLines(latex_table_500, con = here::here("tables/continuous-R-learner", "average_time_metrics_R_500.txt"))

# Output message
cat("Custom LaTeX tables for average time metrics for R = 200 and R = 500 saved as .txt files.")
