# R code to compare propensity scores in time-varying treatment vs continuous treatment
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Create a sample dataset
n <- 500
X1 <- runif(n, 0, 1)
X2 <- runif(n, 0, 1)

# Time points for time-varying propensity score
time_points <- seq(0, 10, by = 0.1)

# Generate data for time-varying propensity score
# Probability of treatment increases with time and depends on covariates
generate_time_varying_ps <- function(x1, x2, t) {
  # Baseline probability increases with time
  baseline <- plogis(-2 + 0.3*t)
  # Covariate effect
  covariate_effect <- 0.5*x1 + 0.3*x2
  # Final probability (bounded between 0 and 1)
  pmin(pmax(baseline + covariate_effect, 0), 1)
}

# Generate data for continuous treatment propensity score
# Probability density over treatment values
generate_continuous_ps <- function(x1, x2, treatment_values) {
  # Mean of distribution depends on covariates
  mu <- 2 + 1.5*x1 + x2
  # Standard deviation
  sigma <- 0.8
  # Return density values
  dnorm(treatment_values, mean = mu, sd = sigma)
}

# Select 3 example individuals with different covariate values
example_ids <- c(1, 250, 500)
example_data <- data.frame(
  id = example_ids,
  X1 = X1[example_ids],
  X2 = X2[example_ids]
)

# Calculate time-varying propensity scores for these individuals
time_varying_ps <- expand.grid(
  id = example_ids,
  time = time_points
) %>%
  left_join(example_data, by = "id") %>%
  mutate(ps = mapply(generate_time_varying_ps, X1, X2, time))

# Calculate continuous treatment propensity scores for these individuals
treatment_values <- seq(0, 6, by = 0.1)
continuous_ps <- expand.grid(
  id = example_ids,
  treatment = treatment_values
) %>%
  left_join(example_data, by = "id") %>%
  mutate(ps = mapply(generate_continuous_ps, X1, X2, treatment))

# Simplified labels for patients
patient_labels <- c("Patient 1", "Patient 2", "Patient 3")

# Plot 1: Time-varying propensity score
p1 <- ggplot(time_varying_ps, aes(x = time, y = ps, color = factor(id))) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "Set1", labels = patient_labels) +
  labs(title = "Time-Varying Propensity Score",
       x = "Time (t)",
       y = "Probability of treatment",
       color = "Subject") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  ylim(0, 1)

# Plot 2: Continuous treatment propensity score
p2 <- ggplot(continuous_ps, aes(x = treatment, y = ps, color = factor(id))) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "Set1", labels = patient_labels) +
  labs(title = "Continuous Treatment Propensity Score",
       x = "Treatment value",
       y = "Density",
       color = "Subject") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Save as two separate images with optimized dimensions
ggsave("figures/time_varying_propensity.png", plot = p1, width = 12, height = 7, dpi = 300)
ggsave("figures/continuous_treatment_propensity.png", plot = p2, width = 12, height = 7, dpi = 300)