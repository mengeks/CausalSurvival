# Load necessary libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
library(here)

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[2])
n <- as.numeric(args[4])
eta_type <- args[6]
HTE_type <- args[8]
tx_difficulty <- args[10]
params_file <- args[12]
verbose <- as.numeric(args[14])

# # Parse command-line arguments
# args <- commandArgs(trailingOnly = TRUE)
# 
# # Debug: Print all arguments
# cat("Command-line arguments:\n")
# print(args)
# 
# # Parse individual arguments
# i <- as.numeric(args[2])
# cat("Parsed i (iteration):", i, "\n")
# 
# n <- as.numeric(args[4])
# cat("Parsed n (sample size):", n, "\n")
# 
# eta_type <- args[6]
# cat("Parsed eta_type (baseline outcome model):", eta_type, "\n")
# 
# HTE_type <- args[8]
# cat("Parsed HTE_type (heterogeneous treatment effect):", HTE_type, "\n")
# 
# tx_difficulty <- args[10]
# cat("Parsed tx_difficulty (treatment difficulty):", tx_difficulty, "\n")
# 
# params_file <- args[12]
# cat("Parsed params_file (parameters file path):", params_file, "\n")
# 
# verbose <- as.numeric(args[14])
# cat("Parsed verbose (verbosity level):", verbose, "\n")



# Load the generate_and_save_data function from the external file
source(here::here("R/datagen-helper.R"))

# Load parameters from JSON file
params <- fromJSON(params_file)
other_params <- params$other_params

# Extract is_time_varying from other_params
is_time_varying <- other_params$is_time_varying

# Combine all parameters
params <- c(list(
  n = n,
  HTE_type = HTE_type,
  eta_type = eta_type,
  tx_difficulty = tx_difficulty,
  is_time_varying = is_time_varying
), other_params)

# This ensures that output_dir is within the /data/ directory in the root folder
# output_dir <- here::here("data", paste0(eta_type, "_", HTE_type))
output_dir <- here::here("data", paste0(eta_type, "_", HTE_type, "_", tx_difficulty))

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Call the generate_and_save_data function
generate_and_save_data(
  i = i,
  n = n,
  path_for_sim_data = output_dir,
  params = params,
  verbose = verbose
)
