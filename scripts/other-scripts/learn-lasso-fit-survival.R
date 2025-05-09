# Load necessary libraries
library(glmnet)
library(ggplot2)

# Set the parameters
set.seed(123)  # For reproducibility
n <- 1000      # Number of observations
d <- 2
# d <- 6         # Dimension of X
sigma <- 1     # Noise level

# Define functions for b*(X) and tau*(X)
b_star <- function(X) {
  sin(pi * X[, 1] * X[, 2])
  # sin(pi * X[, 1] * X[, 2]) + 2 * (X[, 3] - 0.5)^2 + X[, 4] + 0.5 * X[, 5]
}

tau_star <- function(X) {
  (X[, 1] + X[, 2]) / 2
}

# Propensity score function
e_star <- function(X) {
  p <- sin(pi * X[, 1] * X[, 2])
  p <- pmin(pmax(p, 0.1), 0.9)  # Trim to [0.1, 0.9]
  return(p)
}

# Generate data
X <- matrix(runif(n * d, 0, 1), n, d)
colnames(X) <- paste0("X", 1:d)  # Name the columns X1, X2, ..., X6
W <- rbinom(n, 1, e_star(X))
epsilon <- rnorm(n, 0, 1)
Y <- b_star(X) + (W - 0.5) * tau_star(X) + sigma * epsilon

# Combine the data into a data frame
df_fit <- data.frame(Y = Y, W = W, X = X)
# Rename columns to remove the "X." prefix
colnames(df_fit) <- gsub("X\\.", "", colnames(df_fit))

# Now prepare the design matrix using the corrected column names
# X_design <- model.matrix(~ .^2 + I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2) + I(X5^2) + I(X6^2), data = df_fit)[, -1]
X_design <- 
  model.matrix(~ .^2 + 
                 I(X1^2) + 
                 I(X2^2), data = df_fit)[, -1]

# Fit Lasso model
lasso_model <- 
  cv.glmnet(X_design, Y, alpha = 1)  # alpha = 1 for Lasso

# Predict fitted values using Lasso
fitted_values <- 
  predict(lasso_model, 
          newx = X_design, 
          s = "lambda.min")

# Calculate true function values
true_values <- b_star(X) + (W - 0.5) * tau_star(X)

# Combine true and fitted values into a data frame for plotting
df_plot <- data.frame(
  Index = 1:n, 
  TrueValue = true_values, 
  FittedValue = fitted_values[,1])

# Create a ggplot comparing the true and fitted values
p <- ggplot(df_plot, aes(x = Index)) +
  geom_line(aes(y = TrueValue, color = "True Value"), size = 1) +
  geom_line(aes(y = FittedValue, color = "Fitted Value"), size = 1, linetype = "dashed") +
  labs(title = "Comparison of True Function Values and Lasso Fitted Values",
       x = "Index",
       y = "Value") +
  scale_color_manual(values = c("True Value" = "blue", "Fitted Value" = "red")) +
  theme_minimal()

# Print the plot
print(p)
