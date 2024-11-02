source("R/data-handler.R")
source("scripts/TV-CSL/time-varying-estimate.R")
source("scripts/TV-CSL/tests/test-data-handler.R")
source("scripts/TV-CSL/tests/test-helper.R")
library(here)
library(testthat)
library(survival)

#####
# Start 
######
data <- read_TV_CSL_nuisance_data(
  n = 1000,
  eta_type = "10-dim-linear",
  CATE_type = "linear")
fold_nuisance <- data$fold_nuisance
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  load_or_generate_test_data_m_regression(
    n = 1000,
    lambda_C = 0.1,
    eta_type = "10-dim-linear",
    CATE_type = "linear",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 58
  )$train_data
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)
source("scripts/TV-CSL/time-varying-estimate.R")
final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- train_data_original_nuisance$CATE

MSE <- mean((CATE_true - CATE_est)^2)
print("MSE")
print(MSE)


# fold_causal_fitted_id_1 <- fold_causal_fitted %>% filter(id == 1) # always control
# fold_causal_fitted_id_7 <- fold_causal_fitted %>% filter(id == 7) # has a treatment at 0.019384054
# 
# tstop_checker <- fold_causal %>% filter(id == 7)
# df_check_whether_ <- fold_causal_fitted %>% filter(tstop == tstop_checker$tstop[1]) 
# ## more than one row, meaning an adotion time is also used to split the data. 

## n <- unique(fold_causal$tstop)
## for i in  1:n
##  stop_time_i <- stop_time[i]
##  event set = id of whose fold_causal$tstop == stop_time_i and fold_causal$status == 1, both uses 
##  risk set = fold_causal_fitted$tstop == stop_time_i



## to improve the time, give index for tstop, too. Use  index for tstop and user id to search for rows in fold_causal_fitted


## Try manually use 
## 1. Extract the first step -- this makes sense because this is consistent
## 1.1 Let S_lasso to use coxph when both CATE_spec is "linear" and regressor_spec is "linear-only" -- done
first_stage_lasso_ret <- S_lasso(
  train_data = fold_nuisance,
  test_data = fold_causal,
  regressor_spec = "linear-only",
  CATE_spec = "linear" # we hard code this
)


beta_first_stage <- first_stage_lasso_ret$beta_CATE



## 2. fit the optimizer manually (and compare with coxph)
source(here("R/cox-loglik.R"))
source(here::here("tests/test-helper-cox-loglik.R"))

regressors <- cbind(1, fold_causal_fitted %>% select(starts_with("X.")))
interaction_terms <- (fold_causal_fitted$W - fold_causal_fitted$a_t_X) * 
  regressors
source(here("R/cox-loglik.R"))
track_optim <- function(par, start_time, stop_time, status, covar, strata, offset) {
  print("par value: ")
  print(signif(par, 3) )
  
  coxlik_value <- cox_loglik_tv(par, start_time, stop_time, status, covar, strata, offset)
  
  print(paste0("coxlik_value: ", coxlik_value))
  
  return(coxlik_value)
}
result <- optim(par = beta_first_stage, 
                fn = track_optim,
                start_time = fold_causal_fitted$tstart,
                stop_time = fold_causal_fitted$tstop,
                status = fold_causal_fitted$Delta,
                covar = as.matrix(interaction_terms), 
                strata = rep(1, nrow(fold_causal_fitted)), 
                offset = fold_causal_fitted$nu_X,
                method = "BFGS")
print(result)

# next: tract the values that are being optimized -- done
# next: get the coxph solution and compare the likelihood with true values
beta_true <- c(0,1,rep(0,8),1)

loglik_tv_first_stage <- 
  cox_loglik_tv(beta = beta_first_stage, 
                start_time = fold_causal_fitted$tstart,
                stop_time = fold_causal_fitted$tstop,
                status = fold_causal_fitted$Delta,
                covar = as.matrix(interaction_terms), 
                strata = rep(1, nrow(fold_causal_fitted)), 
                offset = fold_causal_fitted$nu_X)

loglik_tv_true <- 
  cox_loglik_tv(beta = beta_true, 
                start_time = fold_causal_fitted$tstart,
                stop_time = fold_causal_fitted$tstop,
                status = fold_causal_fitted$Delta,
                covar = as.matrix(interaction_terms), 
                strata = rep(1, nrow(fold_causal_fitted)), 
                offset = fold_causal_fitted$nu_X)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance,
  beta_CATE_first_stage = beta_first_stage
)

beta_second_stage <- fit_TV_CSL_ret$beta_CATE

loglik_tv_second_stage <- 
  cox_loglik_tv(beta = beta_second_stage, 
                start_time = fold_causal_fitted$tstart,
                stop_time = fold_causal_fitted$tstop,
                status = fold_causal_fitted$Delta,
                covar = as.matrix(interaction_terms), 
                strata = rep(1, nrow(fold_causal_fitted)), 
                offset = fold_causal_fitted$nu_X)

# next: verify that the beta_true give's lowest MSE over all three
CATE_true <- train_data_original_nuisance$CATE
test_regressor_CATE <- cbind(1, as.matrix(train_data_original_nuisance %>% select(starts_with("X."))))

CATE_est_first_stage <- as.vector(test_regressor_CATE %*% beta_first_stage)
(MSE_first_stage <- mean((CATE_true - CATE_est_first_stage)^2))

CATE_est_beta_true<- as.vector(test_regressor_CATE %*% beta_true)
(MSE_beta_true <- mean((CATE_true - CATE_est_beta_true)^2))

CATE_est_second_stage <- as.vector(test_regressor_CATE %*% beta_second_stage)
(MSE_second_stage <- mean((CATE_true - CATE_est_second_stage)^2))


# L2 distances
(RMSE_beta_first_stage <- sqrt(mean((CATE_est_first_stage - CATE_est_beta_true)^2)))
(RMSE_beta_second_stage <- sqrt(mean((CATE_est_second_stage - CATE_est_beta_true)^2)) )

## We have observed that 
##  1. beta_second_stage makes the loglik smaller than that of the first stage -- this is good; 
##  2. MSE_second_stage is larger than that of the MSE_first_stage, and RMSE (distance between the estimated beta and true beta)
##    is larger for the second_stage.
## The above two differs on quality of the estimates. Which should I trust? 
## One explantion is that the loglik (the DR cox loss) is not the right one to optimize which is against my theory
##  My theory is that optimizing on the DR cox loss always give you a better estimate. 


# next: change effect estimate to correct estimates
data <- read_TV_CSL_nuisance_data(
  n = 1000,
  eta_type = "10-dim-linear",
  CATE_type = "linear")
fold_nuisance <- data$fold_nuisance
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  load_or_generate_test_data_m_regression(
    n = 1000,
    lambda_C = 0.1,
    eta_type = "10-dim-linear",
    CATE_type = "linear",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 58
  )$train_data
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)

# the hope here is when we restrict to two dimensional parameters, we could see 
#   improvement of the DML method. The bottleneck is stil speed up the calculation of the 
#   likelihood of TV



# next: change DGP to one dimensional effect instead of linear so the effect does not depend on 

# next: change DGP to 3 independent covariates







# Test TV_CSL for m_regression
data <- read_TV_CSL_nuisance_data(
  n = 500,
  eta_type = "10-dim-linear",
  CATE_type = "linear")
fold_nuisance <- data$fold_nuisance
fold_causal <- data$fold_causal
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- train_data_original_nuisance$CATE

MSE <- mean((CATE_true - CATE_est)^2)
print("MSE")
print(MSE)


data <- read_TV_CSL_nuisance_data(
  n = 500,
  eta_type = "10-dim-linear",
  CATE_type = "linear")
fold_nuisance <- data$fold_nuisance
# fold_causal <- data$fold_causal
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  load_or_generate_test_data_m_regression(
    n = 500,
    lambda_C = 0.1,
    eta_type = "10-dim-linear",
    CATE_type = "linear",
    intercept = 3,
    slope_multiplier = 2.5,
    seed_value = 58
  )$train_data
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- data$train_data_original_nuisance
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- train_data_original_nuisance$CATE

MSE <- mean((CATE_true - CATE_est)^2)
print("MSE")
print(MSE)





#### 
## Try elevating 
n <- 500
eta_type <- "10-dim-linear"
lambda_C = 0.1
seed_value = 123

source("R/datagen-helper.R")
sim_df <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "linear",
  seed_value = seed_value,
  linear_intercept = 3,
  linear_slope_multiplier = 2.5,
  linear_CATE_multiplier = 1, 
  verbose = 0
)

# test: the distribution of eta_0
par(mfrow=c(2,1))
hist(sim_df$eta_0)
hist(sim_df$CATE)
par(mfrow=c(1,1))

# test: the range of non-censored time should not be too small
hist(sim_df$T)



source("R/datagen-helper.R")
sim_df <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "linear",
  seed_value = seed_value,
  linear_intercept = 3,
  linear_slope_multiplier = 2.5,
  linear_CATE_multiplier = 3, 
  verbose = 0
)

# test: the distribution of eta_0
par(mfrow=c(2,1))
hist(sim_df$eta_0)
hist(sim_df$CATE)
par(mfrow=c(1,1))

# test: the range of non-censored time should not be too small
hist(sim_df$T)


source("R/datagen-helper.R")
sim_df <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "linear",
  seed_value = seed_value,
  linear_intercept = -3,
  linear_slope_multiplier = 2.5,
  linear_CATE_multiplier = 3, 
  verbose = 0
)

# test: the distribution of eta_0
par(mfrow=c(2,1))
hist(sim_df$eta_0)
hist(sim_df$CATE)
par(mfrow=c(1,1))

# test: the range of non-censored time should not be too small
hist(sim_df$T)


# test: the proportion of treated should be 
#   between 0.2 and 0.8, ideally be close to 0.5
proportion_W <- sim_df %>%
  mutate(W = A < U) %>%
  summarise(proportion_W = mean(W))
print(proportion_W)
expect_true(proportion_W > 0.2 & proportion_W < 0.8)

# test: the proportion of non-censored should be greater than 0.7
proportion_Delta <- sim_df %>%
  summarise(proportion_Delta = mean(Delta))
print(proportion_Delta)
expect_true(proportion_Delta > 0.7)


### run TV-CSL
fold_nuisance_original <- 
  generate_simulated_data(
    1000, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = 111,
    linear_intercept = 3,
    linear_slope_multiplier = 2.5,
    linear_CATE_multiplier = 3, 
    verbose = 0
  )
fold_nuisance_original <- fold_nuisance_original %>% 
  mutate(U_A = pmin(A,U),
         Delta_A = A <= U)
fold_nuisance <- create_pseudo_dataset(fold_nuisance_original)
source("scripts/TV-CSL/tests/test-helper.R")
fold_causal_original <- 
  generate_simulated_data(
    1000, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = 333,
    linear_intercept = 3,
    linear_slope_multiplier = 2.5,
    linear_CATE_multiplier = 3, 
    verbose = 0
  )
fold_causal <- create_pseudo_dataset(fold_causal_original)
train_data_original_nuisance <- fold_nuisance_original
# tmp <- create_pseudo_dataset(train_data_original_nuisance)

fold_causal_fitted <- TV_CSL_nuisance(
  fold_train = fold_nuisance, 
  fold_test = fold_causal, 
  train_data_original = train_data_original_nuisance,
  prop_score_spec = "cox-linear-all-data",
  lasso_type = "m-regression",
  regressor_spec = "linear-only"
)

final_model_method <- "coxph"
fit_TV_CSL_ret <- fit_TV_CSL(
  fold_causal_fitted = fold_causal_fitted, 
  final_model_method = final_model_method,
  test_data = train_data_original_nuisance
)

fit_TV_CSL_ret$beta_CATE
CATE_est <- fit_TV_CSL_ret$CATE_est
CATE_true <- train_data_original_nuisance$CATE

MSE <- mean((CATE_true - CATE_est)^2)
print("MSE")
print(MSE)




# test: the T distribution for zero CATE is fine

# test: the T distribution for non-linear CATE is fine