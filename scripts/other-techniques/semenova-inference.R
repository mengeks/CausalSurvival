# Simulation Study for Verifying CATE Estimation

# Load required libraries
library(MASS)  # For multivariate normal data generation
library(glmnet)  # For LASSO regression
library(grf)  # For generalized random forests (CATE estimation)

# Simulation parameters
set.seed(123)
n <- 1000   # Sample size
p <- 10     # Number of covariates

generate_data <- function(n, p) {
  # Generate covariates X from a multivariate normal distribution
  X <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
  
  # Generate treatment T and outcome Y
  T <- rbinom(n, 1, plogis(X %*% rnorm(p)))
  Y <- 2 * T + X[, 1] - 0.5 * X[, 2] + rnorm(n)
  
  list(X = X, T = T, Y = Y)
}

# Generate data
data <- generate_data(n, p)
X <- data$X
T <- data$T
Y <- data$Y

# Estimate propensity score (generalized propensity model for binary treatment)
propensity_model <- cv.glmnet(X, T, family = "binomial")
p_hat <- predict(propensity_model, X, s = "lambda.min", type = "response")

# Fit outcome models for treated and control groups
outcome_model_treated <- cv.glmnet(X[T == 1, ], Y[T == 1])
outcome_model_control <- cv.glmnet(X[T == 0, ], Y[T == 0])

mu_hat_treated <- predict(outcome_model_treated, X, s = "lambda.min")
mu_hat_control <- predict(outcome_model_control, X, s = "lambda.min")

# Compute doubly robust scores
dr_scores <- Y - (T * mu_hat_treated + (1 - T) * mu_hat_control) +
  T * (Y - mu_hat_treated) / p_hat - (1 - T) * (Y - mu_hat_control) / (1 - p_hat)

# Fit a linear model for the best linear approximation of CATE
basis_functions <- cbind(X, X^2)  # Example: linear and quadratic terms
lm_model <- lm(dr_scores ~ basis_functions - 1)  # Linear model without intercept

# Results
beta_hat <- coef(lm_model)
cat("Estimated coefficients for the best linear approximation of CATE:\n")
print(beta_hat)

# Verify theoretical results
# Compute standard errors and confidence intervals
dr_variance <- var(residuals(lm_model))
standard_errors <- sqrt(diag(vcov(lm_model)))
conf_intervals <- cbind(beta_hat - 1.96 * standard_errors, beta_hat + 1.96 * standard_errors)

cat("\nConfidence intervals for the coefficients:\n")
print(conf_intervals)

# Plot estimated vs true CATE for selected covariates
true_cate <- 2 + X[, 1] - 0.5 * X[, 2]  # Known CATE structure in simulation
estimated_cate <- basis_functions %*% beta_hat

plot(true_cate, estimated_cate, xlab = "True CATE", ylab = "Estimated CATE",
     main = "True vs Estimated CATE")
abline(0, 1, col = "red", lwd = 2)
