# Load necessary libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
library(here)

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.numeric(args[2])
n <- as.numeric(args[4])
eta_type <- args[6]
CATE_type <- args[8]
params_file <- args[10]
verbose <- as.numeric(args[12])

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
  CATE_type = CATE_type,
  eta_type = eta_type,
  is_time_varying = is_time_varying
), other_params)

# This ensures that output_dir is within the /data/ directory in the root folder
output_dir <- here::here("data", paste0(eta_type, "_", CATE_type))

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
