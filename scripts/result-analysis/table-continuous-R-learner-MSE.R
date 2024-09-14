library(dplyr)
library(tidyr)
library(here)

# Define the path to the CSV file
file_path <- here::here("tables/continuous-R-learner", "aggregated_metrics.csv")

# Read the data
metrics_df <- read.csv(file_path)

# Format values to 3 significant digits
metrics_df <- metrics_df %>%
  mutate(
    Bias = signif(Bias, 3),
    SE_Est = signif(SE_Est, 3),
    MSE = signif(MSE, 3)
  )

# Function to generate a custom LaTeX table for a given R value
generate_custom_latex <- function(R_value) {
  
  # Filter data for the chosen R value
  filtered_df <- metrics_df %>%
    filter(R == R_value) %>%
    arrange(n)
  
  # Create the LaTeX table manually
  latex_table <- "\\begin{table}[ht]\n\\centering\n"
  latex_table <- paste0(latex_table, "\\caption{Performance metrics for R = ", R_value, "}\n")
  latex_table <- paste0(latex_table, "\\begin{tabular}{l|l|ccc|ccc}\n")
  latex_table <- paste0(latex_table, "  \\toprule\n")
  latex_table <- paste0(latex_table, "   \\multirow{2}{*}{n} & \\multirow{2}{*}{Method} & \\multicolumn{3}{c|}{linear-interaction} & \\multicolumn{3}{c}{log} \\\\\n")
  latex_table <- paste0(latex_table, "    &  & Bias & SE & MSE & Bias & SE & MSE \\\\\n")
  latex_table <- paste0(latex_table, "  \\midrule\n")
  
  # Loop over the different sample sizes n
  for (n_value in unique(filtered_df$n)) {
    n_data <- filtered_df %>% filter(n == n_value)
    
    # Add multirow for 'n' and the values for R-Lasso
    r_lasso <- n_data %>% filter(Method == "R-Lasso")
    s_lasso <- n_data %>% filter(Method == "S-Lasso")
    
    latex_table <- paste0(latex_table, "  \\multirow{2}{*}{", n_value, "} & R-Lasso & ", 
                          r_lasso$Bias[1], " & ", r_lasso$SE_Est[1], " & ", r_lasso$MSE[1], " & ",
                          r_lasso$Bias[2], " & ", r_lasso$SE_Est[2], " & ", r_lasso$MSE[2], " \\\\\n")
    
    # Add the values for S-Lasso
    latex_table <- paste0(latex_table, "   & S-Lasso & ", 
                          s_lasso$Bias[1], " & ", s_lasso$SE_Est[1], " & ", s_lasso$MSE[1], " & ",
                          s_lasso$Bias[2], " & ", s_lasso$SE_Est[2], " & ", s_lasso$MSE[2], " \\\\\n")
    
    latex_table <- paste0(latex_table, "  \\midrule\n")
  }
  
  # Close the table
  latex_table <- paste0(latex_table, "  \\bottomrule\n\\end{tabular}\n\\end{table}\n")
  
  return(latex_table)
}

# Generate LaTeX table for R = 200 and save it as a text file
latex_table_200 <- generate_custom_latex(R_value = 200)
writeLines(latex_table_200, con = here::here("tables/continuous-R-learner", "performance_metrics_R_200.txt"))

# Generate LaTeX table for R = 500 and save it as a text file
latex_table_500 <- generate_custom_latex(R_value = 500)
writeLines(latex_table_500, con = here::here("tables/continuous-R-learner", "performance_metrics_R_500.txt"))

# Output message
cat("Custom LaTeX tables for R = 200 and R = 500 saved as .txt files.")



