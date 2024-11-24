suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
library(here)
source(here("scripts", "heart-transplant-datagen", "gen-heart-transplant-sim.R") )
output_dir <- here("data", "heart-transplant-sim")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


generate_and_save_data_heart_transplant <- 
  function(i, 
           path_for_sim_data, 
           verbose = 0) {
    
    start_time <- Sys.time()  # Start timing
    
    seed_value <- 123 + 11 * i
    print(seed_value)
    # params <- list()  # Initialize params as a list
    # params$seed <- seed_value  # Assign seed_value to params
    
    # Print iteration details
    cat("Iteration:", i, 
        "\n")
    
    simulated_data_i <- generate_heart_transplant_simulated_data(seed_value = seed_value)
      
    dataset_with_params <- list(
      data = simulated_data_i,
      # params = params,
      seed_value = seed_value
    )
    
    file_name <- paste0("sim_data_", i, ".rds")
    file_path <- file.path(path_for_sim_data, file_name)
    saveRDS(dataset_with_params, file_path)
    
    
    end_time <- Sys.time()  # End timing
    time_used <- end_time - start_time  # Calculate time used
    
    # Print time taken
    cat("Saved dataset", i, "to", file_path, "\n")
    cat("Time used:", time_used, "\n")
  }

for (i in 1:200){
  generate_and_save_data_heart_transplant(i=i, 
                path_for_sim_data = output_dir)
}
