# Separate script for Test 4

# Load required packages and source the cox-loglik script
library(survival)
source("R/cox-loglik.R")
source("R/datagen-helper.R")

R = 200; i <- 1
sim_data <- 
  read_single_simulation_data(
    n = 500, R = R, 
    is_time_varying = FALSE, i = i)

single_data <- sim_data$data
params <- sim_data$params

# true_beta <- c(1, rep(1, 5), 0.5)
true_beta <- c(params$tau, params$beta, params$delta)

# Combine covariates into a matrix
covar_matrix <- cbind(
  single_data$W, 
  single_data$X.1, 
  single_data$X.2, 
  single_data$X.3, 
  single_data$X.4, 
  single_data$X.5, 
  single_data$X.1 * single_data$X.2
  )

# Fit the custom Cox model
# init_beta <- rep(0, length(true_beta))
init_beta <- true_beta
source("R/cox-loglik.R")
result_custom <- 
  fit_custom_cox_model(
  init_beta = init_beta, 
  time = single_data$U, 
  status = single_data$Delta, 
  covar = covar_matrix, 
  strata = rep(1, length(single_data$U)))

# Output the results for Test 4
cat("Optimized beta estimates from custom model:\n")
print(result_custom$par)

cat("Difference between estimated beta and true beta:\n")
print(result_custom$par - true_beta)

# Compare with coxph
data_for_coxph <- data.frame(
  time = single_data$U, 
  status = single_data$Delta, 
  W = single_data$W, 
  X.1 = single_data$X.1, 
  X.2 = single_data$X.2, 
  X.3 = single_data$X.3, 
  X.4 = single_data$X.4, 
  X.5 = single_data$X.5, 
  interaction = single_data$X.1 * single_data$X.2)

coxph_formula <- 
  Surv(time, status) ~ 
  W + X.1 + X.2 + X.3 + X.4 + X.5 + interaction
coxph_fit <- coxph(
  coxph_formula, data = data_for_coxph)

cat("Beta estimates from coxph model:\n")
print(coef(coxph_fit))

cat("Difference between custom model beta and coxph beta:\n")
print(result_custom$par - coef(coxph_fit))

cat("Log-likelihood at the optimum (custom model):\n")
print(-result_custom$value)
