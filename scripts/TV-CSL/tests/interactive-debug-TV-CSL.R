source("R/data-handler.R")
source("scripts/TV-CSL/time-varying-estimate.R")
source("scripts/TV-CSL/tests/test-data-handler.R")
source("scripts/TV-CSL/tests/test-helper.R")
source(here("R/cox-loglik.R"))
library(here)
library(testthat)
library(survival)

fold_nuisance_original <- 
  read_single_simulation_data(n = 500, 
                              is_time_varying=T, 
                              i = 1,
                              eta_type = "linear", 
                              HTE_type = "linear")$data
fold_nuisance_original <- fold_nuisance_original %>% 
  mutate(U_A = pmin(A,U),
         Delta_A = A <= U)
fold_nuisance <- create_pseudo_dataset(survival_data = fold_nuisance_original)
fold_causal_original <- test_data <- 
  read_single_simulation_data(n = 500, 
                              is_time_varying=T, 
                              i = 101,
                              eta_type = "linear", 
                              HTE_type = "linear")$data
fold_causal <- create_pseudo_dataset(survival_data = fold_causal_original) 

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = fold_nuisance_original,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "S-lasso",
  regressor_spec = "linear",
  HTE_spec = "linear"
)

large_nuisance_set <- fold_causal_fitted$fold_test_final
small_nuisance_set <- fold_causal


covariates <- fold_test_final %>% select(starts_with("X."))
all_regressors <- cbind(covariates, cbind(1, covariates) * fold_test_final$W )

num_covariates <- 3
covariate_terms <- paste("X.", 1:num_covariates, sep = "", collapse = " + ")
interaction_terms <- paste("X.", 1:num_covariates, sep = "", collapse = " + ")
interaction_formula <- paste("W * (1 +", interaction_terms, ")")
full_formula <- paste("Surv(tstart, tstop, Delta) ~", covariate_terms, "+", interaction_formula)

m_small_set <- coxph(as.formula(full_formula), data = small_nuisance_set)
(beta_small_set <- coef(m_small_set))
m_large_set <- coxph(as.formula(full_formula), data = large_nuisance_set)
(beta_large_set <- coef(m_large_set))

loglik_comparison_beta <- function(df) {
  
  covariates <- df %>% select(starts_with("X."))
  all_regressors <- cbind(covariates, cbind(1, covariates) * df$W)
  
  
  loglik_beta_small_set <- cox_loglik_tv(
    beta = beta_small_set, 
    start_time = df$tstart,
    stop_time = df$tstop,
    status = df$Delta,
    covar = as.matrix(all_regressors), 
    strata = rep(1, nrow(df)), 
    offset = 0
  )
  
  cat("Beta small set is:", beta_small_set, "\n")
  cat("Log-likelihood small set beta is:", loglik_beta_small_set, "\n")
  
  loglik_beta_large_set <- cox_loglik_tv(
    beta = beta_large_set, 
    start_time = df$tstart,
    stop_time = df$tstop,
    status = df$Delta,
    covar = as.matrix(all_regressors), 
    strata = rep(1, nrow(df)), 
    offset = 0
  )
  
  cat("Beta large set is:", beta_large_set, "\n")
  cat("Log-likelihood large set beta is:", loglik_beta_large_set, "\n")
}

loglik_comparison_beta(df = small_nuisance_set)


## Debug:
loglik_comparison_sets <- function(beta, small_nuisance_set, large_nuisance_set) {
  
  covariates <- small_nuisance_set %>% select(starts_with("X."))
  all_regressors <- cbind(covariates, cbind(1, covariates) * small_nuisance_set$W)
  
  cat("Beta is:", beta, "\n")
  
  loglik_small_set <- cox_loglik_tv(
    beta = beta, 
    start_time = small_nuisance_set$tstart,
    stop_time = small_nuisance_set$tstop,
    status = small_nuisance_set$Delta,
    covar = as.matrix(all_regressors), 
    strata = rep(1, nrow(small_nuisance_set)), 
    offset = 0
  )
  
  cat("Log-likelihood small set is:", loglik_small_set, "\n")
  
  
  covariates <- large_nuisance_set %>% select(starts_with("X."))
  all_regressors <- cbind(covariates, cbind(1, covariates) * large_nuisance_set$W)
  
  cat("Beta is:", beta, "\n")
  
  loglik_large_set <- cox_loglik_tv(
    beta = beta, 
    start_time = large_nuisance_set$tstart,
    stop_time = large_nuisance_set$tstop,
    status = large_nuisance_set$Delta,
    covar = as.matrix(all_regressors), 
    strata = rep(1, nrow(large_nuisance_set)), 
    offset = 0
  )
  
  cat("Log-likelihood large set is:", loglik_large_set, "\n")
}

loglik_comparison_sets(beta = beta_large_set, 
                       small_nuisance_set = small_nuisance_set, 
                       large_nuisance_set = large_nuisance_set)
## this is problematic --- the likelihoods should be equal
# The guess is that the separation procedure has issue 
# Example dataset
df <- data.frame(
  id = c(1, 1, 2, 2),
  tstart = c(0, 8, 0, 5),
  tstop = c(8, 14, 5, 12),
  W = c(0, 1, 0, 1),
  Delta = c(0, 1, 0, 0),
  X.1 = c(0.5, 0.5, 0.2, 0.2),
  X.2 = c(-0.5, -0.5, 0.1, 0.1),
  X.3 = c(0.3, 0.3, 0.8, 0.8)
)

# Display the dataset
print(df)

## After a transformation, it should become
df_transformed_expected <- data.frame(
  id = c(1, 1, 1, 1, 2, 2, 2),
  tstart = c(0, 5, 8, 12, 0, 5, 8),
  tstop = c(5, 8, 12, 14, 5, 8, 12),
  W = c(0, 0, 1, 1, 0, 1, 1),
  Delta = c(0, 0, 0, 1, 0, 0, 0),
  X.1 = c(0.5, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2),
  X.2 = c(-0.5, -0.5, -0.5, -0.5, 0.1, 0.1, 0.1),
  X.3 = c(0.3, 0.3, 0.3, 0.3, 0.8, 0.8, 0.8)
)

print(df_transformed_expected)

split_within_intervals <- function(row, cut_points) {
  original_tstart <- row[["tstart"]]
  original_tstop <- row[["tstop"]]
  
  valid_cuts <- cut_points[cut_points > original_tstart & cut_points < original_tstop]
  final_cuts <- sort(c(original_tstart, valid_cuts, original_tstop))
  
  new_intervals <- data.frame(
    tstart = head(final_cuts, -1),
    tstop = tail(final_cuts, -1),
    id = row[["id"]],
    W = row[["W"]]
  )
  return(new_intervals)
}

granular_cut_points <- unique(df$tstop)
fold_test_split <- map_dfr(seq_len(nrow(df)), function(i) {
  split_within_intervals(df[i, ], granular_cut_points)
})

df_transformed <- 
  left_join(fold_test_split, 
            df %>% select(-tstart, -tstop), 
            by = c("id", "W") )

df_transformed <- df_transformed[, colnames(df_transformed_expected)]
(comparison <- all.equal(df_transformed, df_transformed_expected))

# We found a difference! df_transformed wrongly carried delta of the end period to all of the granular points
# We should manually adjust by 

df_transformed_modified <- 
  left_join(fold_test_split, 
            df %>% select(-tstart, -tstop), 
            by = c("id", "W") )
## For each id in df_transformed_modified, arrange the rows by ascending order of tstop
##  Set Delta to 0 for all rows before the last tstop
df_transformed_modified <- df_transformed_modified %>%
  arrange(id, tstop)

df_transformed_modified <- df_transformed_modified %>%
  group_by(id) %>%
  mutate(Delta = if_else(tstop < max(tstop), 0, Delta)) %>%
  ungroup()
df_transformed_modified <- df_transformed_modified[, colnames(df_transformed_expected)]
(comparison <- all.equal(df_transformed_modified, df_transformed_expected))


## Make sure the test is good
loglik_comparison_sets(beta = beta_large_set, 
                       small_nuisance_set = df, 
                       large_nuisance_set = df_transformed_expected)


## Now test the second 
lasso_ret_first_stage = fold_causal_fitted$lasso_ret
tmp <- fit_TV_CSL(fold_causal_fitted = fold_causal_fitted$fold_test_final, 
                       final_model_method="coxph", 
                       test_data = test_data, 
                       beta_HTE_first_stage = lasso_ret_first_stage$beta_HTE,
                       HTE_spec = "linear")

