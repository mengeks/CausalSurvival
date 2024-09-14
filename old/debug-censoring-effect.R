setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")
library(tidyverse)
source("cox-utils.R")

# Parameters
n <- 2000  # Number of observations
alpha <- 1.5
light_censoring = T
if (light_censoring == F){
  lambda_C <- 0.1
}
lambda_0 <- 1
tau <- 1
beta <- 1
beta_2 <- 2  # Coefficient for the non-linear term
R <- 100

simulated_data <-
  generate_simulated_data(
  is_time_varying=F, 
  is_linear=F,
  light_censoring = light_censoring
)

## LASSO implementation

cox_lasso <- 
  glmnet::glmnet(
    cbind(1,simulated_data$X), 
     Surv(simulated_data$U, simulated_data$Delta), 
         family = "cox", 
         alpha = 1)  # alpha = 1 for Lasso
summary(cox_lasso)

best_lambda <- 
  cox_lasso$lambda.min
coefficients <- 
  coef(cv_fit, s = best_lambda)


effect_true <- 
  coxph(Surv(U, Delta) ~ 1 + X + I(X^2) + W, 
        data = simulated_data, 
        ties = "breslow")
summary(effect_true)$coefficients
effect_wrong <- 
  coxph(Surv(U, Delta) ~ 1 + X + W, 
        data = simulated_data, 
        ties = "breslow")
summary(effect_wrong)$coefficients

set.seed(123)
n_split_runs <- 100
DINA_results <- numeric(n_split_runs)
for (i in 1:n_split_runs){
  model_DINA <- 
    non_time_varying_estimate(
      simulated_data,
      causal=T,
      light_censoring=T)
  DINA_results[i] <-
    summary(model_DINA)$coefficients[1,1]
}
hist(DINA_results)
mean(DINA_results)
sd(DINA_results)


# control_data <- simulated_data %>% filter(W == 0)
# treated_data <- simulated_data %>% filter(W == 1)
# mean(control_data$T)
# mean(treated_data$T)
# eta_0 <- 
#   coxph(Surv(U, Delta) ~ 1 + X, 
#         data = simulated_data %>% filter(W == 0), 
#         ties = "breslow")
# summary(eta_0)$coefficients
# eta_0_true <- 
#   coxph(Surv(U, Delta) ~ 1 + X + I(X^2), 
#         data = simulated_data %>% filter(W == 0), 
#         ties = "breslow")
# summary(eta_0_true)$coefficients
n = 100; R = 200
run_result(
  is_linear=F,
  is_time_varying=F,
  causal=T,
  light_censoring = T
)
# n = 100; R = 200: time 116s = 2mins

n = 100; R = 200
run_result(
  is_linear=F,
  is_time_varying=F,
  causal=F,
  light_censoring = T
)
# n = 100; R = 200: time 116s = 2mins

report_results(
  is_linear=F, 
  causal=T,
  n, R, data_folder, plot_folder,
  light_censoring = T
)
