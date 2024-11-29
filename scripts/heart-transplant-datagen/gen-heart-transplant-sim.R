
generate_heart_transplant_simulated_data <- 
  function(max_censoring_time = 1000,
           seed_value = 123,
           verbose = 0) {
    
    verbose_print <- function(message, level = 1) {
      if (verbose >= level) {
        cat(message, "\n")
      }
    }
    verbose_print("Step 0: Setting seed", 2)
    set.seed(seed_value)
    
    # load dataset 
    library(here)
    library(tidyverse)
    load(file=here("scripts/heart-transplant-analysis/data/stanford-RHC-processed.rds"))
    
    # Estimate eta0 and HTE
    df_time_var <- df_time_var %>%
      mutate(
        X.1 = scale(age),        # Standardize age
        X.2 = scale(year),       # Standardize year
        Delta = death,
        W = trt
      )
    
    # Standardizing X.1 (age) and X.3 (year) in df_original
    df_original <- df_original %>%
      mutate(
        X.1 = scale(age),        # Standardize age
        X.2 = scale(year)        # Standardize year
      )
    df_original <- df_original %>% 
      mutate(U_A = pmin(txtime,futime),
             Delta_A = txtime <= futime,
             Delta = death)
    
    
    source("scripts/TV-CSL/time-varying-estimate.R")
    source("R/data-handler.R")
    K <- 2
    TV_CSL_ret_linear_eta <- TV_CSL(train_data = df_time_var, 
                                    test_data = df_original, 
                                    train_data_original = df_original, 
                                    K =  K, 
                                    prop_score_spec = "cox-linear-all-data",
                                    lasso_type = "S-lasso",
                                    eta_type = "non-linear",
                                    HTE_type = "linear",
                                    regressor_spec = "linear",
                                    # regressor_spec = "complex",
                                    HTE_spec = "linear",
                                    final_model_method = "lasso_coxph",
                                    id_var = "subject",
                                    lasso_warmstart = F,
                                    verbose = 0) 
    ### Obtain the HTE estimate 
    beta_HTE <- TV_CSL_ret_linear_eta$beta_HTE
    
    ### Obtain the eta0 estimate 
    n.covar <- ncol(df_original %>% select(starts_with("X.")))
    beta_eta_0s <- array(dim = c(n.covar, K))
    for (k in 1:K){
      first_stage_lasso_k <- TV_CSL_ret_linear_eta$first_stage_lassos[[k]]
      beta_eta_0s[,k] <- first_stage_lasso_k$beta_eta_0
    }
    
    beta_eta_0 <- rowMeans(beta_eta_0s, na.rm = TRUE)
    
    
    ### Put eta_0 estimate and HTE estimate on the dataset
    df_original_sim <- df_original
    df_original_sim <- df_original_sim %>% 
      rowwise() %>%  
      mutate(
        eta_0 = sum(beta_eta_0 * c_across(starts_with("X."))),  # Compute eta0 for each row
        HTE = sum(beta_HTE * c(1, c_across(starts_with("X."))))  # Compute HTE for each row
      ) %>%
      ungroup() 
    
    
    ## Simulate the propensity score:
    ## First we estimate the propensity score using year (X.2) only
    covariate_terms <- "X.2"
    formula <- as.formula(paste("Surv(U_A, Delta_A) ~", covariate_terms))
    treatment_model <- coxph(formula, 
                         data = df_original, 
                         ties = "breslow")
    alpha_estimate <- treatment_model$coefficients
    ## Then we generate the treatment time based on the alpha_estimate
    # Compute the baseline hazard from the Cox model
    baseline_hazard <- basehaz(treatment_model, centered = FALSE)
    
    H_inverse <- approxfun(baseline_hazard$hazard, baseline_hazard$time)
    n <- nrow(df_original)  # Number of observations
    U <- runif(n)
    
    linear_predictor <- as.numeric(df_original[, covariate_terms] * alpha_estimate)
    
    treatment_times <- H_inverse(-log(U) / exp(linear_predictor))
    df_original_sim$A <- treatment_times
    df_original_sim <- df_original_sim %>% filter(!is.na(A))
    
    # # Generated A is quite similar to the original treated time!
    # boxplot(df_original_sim$A,df_original_sim$txtime)
    
    source("R/datagen-helper.R")
    verbose_print("Step 3: Defining the baseline hazard function...", 2)
    hazard_function <- function(t, x, betas, ...) {
      verbose_print(paste0("baseline_type passed to hazard_function is:    ", baseline_type), 2) 
      calculate_hazard(
        t, x,
        is_time_varying = T, 
        baseline_type = "constant")
    }
    
    verbose_print("Step 4: Simulating survival data...", 2)
    start_time <- Sys.time()
    df_original_sim_eta_edit <- df_original_sim %>%
      # mutate(eta_0 = eta_0 - 10)
      mutate(eta_0 = eta_0 - 5)
    source("R/datagen-helper.R")
    simulated_data <- simsurv(
      hazard = hazard_function,
      x = df_original_sim_eta_edit,
      interval = c(1e-22, 1500),
      maxt = max_censoring_time
    )
    end_time <- Sys.time()
    verbose_print(sprintf("Survival data simulated in %.2f seconds.", as.numeric(end_time - start_time)), 1)
    
    # boxplot(simulated_data$eventtime,df_original_sim$futime)
    
    start_time <- Sys.time()
    # original data has 32% event; 
    # We do this by random censoring till 
    # end i want to have slightly larger 50% event. 
    lambda_C <- 1/mean(simulated_data$eventtime)
    C_gen <- 
      generate_censoring_times(n = nrow(df_original_sim_eta_edit), light_censoring = F, lambda_C)
    C <- pmin(max_censoring_time, C_gen)
    end_time <- Sys.time()
    verbose_print(sprintf("Censoring times generated in %.2f seconds.", as.numeric(end_time - start_time)), 1)
    
    df_original_sim_eta_edit <- df_original_sim_eta_edit %>%
      mutate(id = 1:n() )
    processed_data <- 
      post_process(simulated_data = simulated_data, 
                   covariates = df_original_sim_eta_edit, 
                   C = C, 
                   C_gen = C_gen,
                   verbose = verbose)
    
    verbose_print("Data generation completed.", 1)
    
    
    
    return(processed_data)
  }

# s# Examples
# df_123 <- generate_heart_transplant_simulated_data(seed_value = 123)
# # # proportion of treated
# table(df_123$U > df_123$A) # 67%
# # # proportion of death
# table(df_123$Delta) # 49%
# 
# df_456 <- generate_heart_transplant_simulated_data(seed_value = 456)
# table(df_456$U > df_456$A) # 96%
# table(df_456$Delta) # 36%
