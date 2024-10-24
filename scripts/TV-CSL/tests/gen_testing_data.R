library(dplyr)
library(here)

generate_TV_CSL_nuisance_data <- function(n = 500, i = 1, eta_type = "10-dim-non-linear", CATE_type = "linear", k = 1, save_path = here("scripts/TV-CSL/tests/data")) {
  
  source("R/data-handler.R")
  source("scripts/TV-CSL/time-varying-estimate.R")
  
  train_data_original <- read_single_simulation_data(n = n, i = i, eta_type = eta_type, CATE_type = CATE_type)$data
  test_data <- read_single_simulation_data(n = n, i = i + 100, eta_type = eta_type, CATE_type = CATE_type)$data
  
  train_data <- create_pseudo_dataset(survival_data = train_data_original)
  
  train_data_original <- train_data_original %>%
    mutate(U_A = pmin(A, U), Delta_A = A <= U)
  
  folds <- cut(seq(1, n), breaks = 5, labels = FALSE)
  
  nuisance_ids <- train_data_original[folds != k, "id"]
  causal_ids <- train_data_original[folds == k, "id"]
  
  fold_nuisance <- train_data[train_data$id %in% nuisance_ids, ]
  fold_causal <- train_data[train_data$id %in% causal_ids, ]
  
  train_data_original_nuisance <- train_data_original[train_data_original$id %in% nuisance_ids, ]
  
  save_file_path <- file.path(save_path, paste0("TV_CSL_nuisance_data_k", k, ".RData"))
  save(fold_nuisance, fold_causal, train_data_original_nuisance, file = save_file_path)
  
  message("Data saved to: ", save_file_path)
}

generate_TV_CSL_nuisance_data(n = 500, i = 1, eta_type = "10-dim-non-linear", CATE_type = "linear", k = 1)
