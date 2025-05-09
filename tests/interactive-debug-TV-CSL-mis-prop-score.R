source("R/data-handler.R")
source("scripts/TV-CSL/time-varying-estimate.R")
source("scripts/TV-CSL/tests/test-data-handler.R")
source("scripts/TV-CSL/tests/test-helper.R")
source(here("R/cox-loglik.R"))
library(here)
library(testthat)
library(survival)

n = 200
K = 2
eta_type = "non-linear"
HTE_type = "linear"
prop_score_spec = "cox-linear-mis-specification"
HTE_spec = "linear"
train_data_original <- 
  read_single_simulation_data(n = n, 
                              is_time_varying=T, 
                              i = 65,
                              eta_type = eta_type, 
                              HTE_type = HTE_type)$data
train_data_original <- train_data_original %>% 
  mutate(U_A = pmin(A,U),
         Delta_A = A <= U)
train_data <- create_pseudo_dataset(survival_data = train_data_original)
test_data <- 
  read_single_simulation_data(n = n, 
                              is_time_varying=T, 
                              i = 66,
                              eta_type = eta_type, 
                              HTE_type = HTE_type)$data
source("scripts/TV-CSL/time-varying-estimate.R")
timing <- system.time({
  TV_CSL_ret_s_lasso_eta_complex_coxph <- TV_CSL(train_data = train_data, 
                                                 test_data = test_data, 
                                                 train_data_original = train_data_original, 
                                                 HTE_type = HTE_type,
                                                 eta_type = eta_type,
                                                 K = K, 
                                                 prop_score_spec = prop_score_spec, 
                                                 lasso_type = "S-lasso", 
                                                 regressor_spec = "complex", 
                                                 HTE_spec = HTE_spec,
                                                 i = 65,
                                                 final_model_method = "lasso_coxph",
                                                 verbose = 0)
})

timing <- system.time({
  TV_CSL_ret_s_lasso_eta_linear_coxph <- TV_CSL(train_data = train_data, 
                                                test_data = test_data, 
                                                train_data_original = train_data_original, 
                                                HTE_type = HTE_type,
                                                eta_type = eta_type,
                                                K = K, 
                                                prop_score_spec = prop_score_spec, 
                                                lasso_type = "S-lasso", 
                                                regressor_spec = "linear", 
                                                HTE_spec = HTE_spec,
                                                i = 65,
                                                final_model_method = "lasso_coxph",
                                                verbose = 0)
})
TV_CSL_ret_s_lasso_eta_complex_coxph$beta_HTE
TV_CSL_ret_s_lasso_eta_linear_coxph$beta_HTE
TV_CSL_ret_s_lasso_eta_complex_coxph$beta_HTE - TV_CSL_ret_s_lasso_eta_linear_coxph$beta_HTE


timing <- system.time({
  TV_CSL_ret_m_eta_complex_coxph <- TV_CSL(train_data = train_data, 
                                           test_data = test_data, 
                                           train_data_original = train_data_original, 
                                           HTE_type = HTE_type,
                                           eta_type = eta_type,
                                           K = K, 
                                           prop_score_spec = prop_score_spec, 
                                           lasso_type = "m-regression", 
                                           regressor_spec = "complex", 
                                           HTE_spec = HTE_spec,
                                           i = 65,
                                           final_model_method = "lasso_coxph",
                                           verbose = 0)
})

timing <- system.time({
  TV_CSL_ret_m_eta_linear_coxph <- TV_CSL(train_data = train_data, 
                                          test_data = test_data, 
                                          train_data_original = train_data_original, 
                                          HTE_type = HTE_type,
                                          eta_type = eta_type,
                                          K = K, 
                                          prop_score_spec = prop_score_spec, 
                                          lasso_type = "m-regression", 
                                          regressor_spec = "linear", 
                                          HTE_spec = HTE_spec,
                                          i = 65,
                                          final_model_method = "lasso_coxph",
                                          verbose = 0)
})
