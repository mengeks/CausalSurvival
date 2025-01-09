# Set parameters
set.seed(42)  # for reproducibility
n <- 1000  # number of observations
beta_0 <- 1.0  # intercept
beta_1 <- 2.0  # coefficient for X1

# Function to simulate and calculate MSE
simulate_mse <- function(n, beta_0, beta_1) {
  # Simulate data
  X1 <- rnorm(n, 0, 1)
  X2 <- rnorm(n, 0, 1)
  X3 <- rnorm(n, 0, 1)
  
  # True model
  Y <- beta_0 + beta_1 * X1 + rnorm(n, 0, 1)  # add noise
  
  # Fit the mis-specified model Y ~ X2 + X3
  mispecified_model <- lm(Y ~ X2 + X3)
  
  # Predicted values from the mis-specified model
  Y_pred <- predict(mispecified_model)
  
  # Calculate MSE between true Y and fitted values
  mse <- mean((Y - Y_pred)^2)
  return(mse)
}

# Run the simulation R times
R <- 500
mse_values <- replicate(R, simulate_mse(n, beta_0, beta_1))

# Summary statistics of MSE
cat("Mean of MSE over", R, "simulations:", mean(mse_values), "\n")
cat("Standard deviation of MSE:", sd(mse_values), "\n")

# Plot the distribution of MSE values
hist(mse_values, main = "Distribution of MSE", xlab = "MSE", breaks = 30, col = "lightblue")





library(survival)

# Set parameters
set.seed(42)  # for reproducibility
n <- 1000  # number of observations

# Simulate data
X1 <- rnorm(n, 0, 1)
X2 <- rnorm(n, 0, 1)
X3 <- rnorm(n, 0, 1)

# Simulate treatment times A_i based on Exponential distribution
lambda <- exp(X2 + X3)
A_i <- rexp(n, rate = lambda)

# Fit a Cox proportional hazards model
cox_model <- coxph(Surv(A_i, rep(1, n)) ~ X1)
alpha_estimate <- coef(cox_model)

# Function to calculate treatment probabilities
calculate_eX <- function(alpha_estimate, X, t) {
  hazard_adapt_date <- exp(X %*% alpha_estimate)
  trt_prob <- 1 - exp(-t * hazard_adapt_date)
  return(trt_prob)
}

# Estimate treatment probabilities
X <- cbind(X1)
t_values <- seq(min(A_i), max(A_i), length.out = 100)
fitted_cdf <- sapply(t_values, function(t) mean(calculate_eX(alpha_estimate, X, t)))

# Calculate actual CDF
actual_cdf <- sapply(t_values, function(t) mean(A_i <= t))

# Plot the actual CDF and the fitted CDF
plot(t_values, actual_cdf, type = "l", col = "blue", lwd = 2, 
     ylab = "Cumulative Probability", xlab = "Time", 
     main = "Actual CDF vs Fitted CDF")
lines(t_values, fitted_cdf, col = "red", lwd = 2)
legend("bottomright", legend = c("Actual CDF", "Fitted CDF"), 
       col = c("blue", "red"), lwd = 2)
