library(dplyr)
library(here)

generate_m_estimation_data <- function(){
  
  sim_constant <- generate_simulated_data(
    n = 200, 
    lambda_C = 0.1,
    eta_type = "10-dim-linear",
    CATE_type = "constant",
    seed_value = 123,
    verbose = 0
  ) %>% mutate(W = A < U)
  
}

create_TV_CSL_nuisance_filepath <- function(save_path, k, eta_type) {
  file.path(save_path, 
            paste0("TV_CSL_nuisance_data_k", k,
                   ifelse(eta_type == "10-dim-linear", "_eta-type-10-dim-linear", ""),
                   ".RData"))
}

generate_TV_CSL_nuisance_data <- 
  function(n = 500, 
           i = 1, 
           eta_type = "10-dim-non-linear", 
           CATE_type = "linear", 
           k = 1, 
           save_path = here("scripts/TV-CSL/tests/data")) {
  
  source("R/data-handler.R")
  source("scripts/TV-CSL/time-varying-estimate.R")
  source("scripts/TV-CSL/tests/test-helper.R")
  
    if (eta_type == "10-dim-non-linear") {
      train_data_original <- 
        read_single_simulation_data(
          n = n, i = i, eta_type = eta_type, CATE_type = CATE_type)$data
      test_data <- 
        read_single_simulation_data(
          n = n, 
          i = i + 100, 
          eta_type = eta_type, 
          CATE_type = CATE_type)$data
    } else if (eta_type == "10-dim-linear") {
      train_data_original <- test_data <- 
        load_or_generate_test_data_m_regression(
          n = n,
          lambda_C = 0.1,
          eta_type = "10-dim-linear",
          # CATE_type = "linear",
          CATE_type = CATE_type,
          intercept = 3,
          slope_multiplier = 2.5,
          seed_value = 42
        )$train_data
    }
  
  train_data <- 
    create_pseudo_dataset(survival_data = train_data_original)
  
  train_data_original <- train_data_original %>%
    mutate(U_A = pmin(A, U), Delta_A = A <= U)
  
  folds <- cut(seq(1, n), breaks = 5, labels = FALSE)
  
  nuisance_ids <- train_data_original[folds != k, "id"]
  causal_ids <- train_data_original[folds == k, "id"]
  
  fold_nuisance <- train_data[train_data$id %in% nuisance_ids, ]
  fold_causal <- train_data[train_data$id %in% causal_ids, ]
  
  train_data_original_nuisance <- train_data_original[train_data_original$id %in% nuisance_ids, ]
  
  dir.create(save_path, recursive = T, showWarnings = F)
  save_file_path <- 
    create_TV_CSL_nuisance_filepath(
      save_path=save_path, 
      k = k, 
      eta_type = eta_type)
  
  
  save(fold_nuisance, fold_causal, train_data_original_nuisance, file = save_file_path)
  
  message("Data saved to: ", save_file_path)
}

read_TV_CSL_nuisance_data <- 
 function(k = 1, 
          data_path = here("scripts/TV-CSL/tests/data"),
          n = 500,
          i = 1,
          eta_type = "10-dim-non-linear",
          CATE_type = "linear") {
   
   file_path <- create_TV_CSL_nuisance_filepath(data_path, k, eta_type)
   
   if (!file.exists(file_path)) {
     data <- generate_TV_CSL_nuisance_data(
       k = k,
       n = n,
       i = i,
       eta_type = eta_type,
       CATE_type = CATE_type,
       save_path = data_path
     )
     return(data)
   }
   
   load(file_path)
   
   list(
     fold_nuisance = fold_nuisance,
     fold_causal = fold_causal,
     train_data_original_nuisance = train_data_original_nuisance
   )
 }

read_TV_CSL_nuisance_data <- 
  function(k = 1, 
           data_path = here("scripts/TV-CSL/tests/data"),
           n = 500,
           i = 1,
           eta_type = "10-dim-non-linear",
           CATE_type = "linear") {
    
    file_path <- create_TV_CSL_nuisance_filepath(data_path, k, eta_type)
    
    if (!file.exists(file_path)) {
      data <- generate_TV_CSL_nuisance_data(
        k = k,
        n = n,
        i = i,
        eta_type = eta_type,
        CATE_type = CATE_type,
        save_path = data_path
      )
      return(data)
    }
    
    load(file_path)
    
    list(
      fold_nuisance = fold_nuisance,
      fold_causal = fold_causal,
      train_data_original_nuisance = train_data_original_nuisance
    )
  }
