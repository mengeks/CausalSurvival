source("datagen-helper.R") 
m_timevar <- 
  coxph(formula = 
          Surv(U,Delta) ~ 
          W + X,
        data = simulated_data, 
        ties = "breslow")
m_timevar_summary <- 
  summary(m_timevar)
tau_estimate <- 
  m_timevar_summary$coefficients[1,1]
eta_0_estimate <- 
  m_timevar_summary$coefficients[2,1]
print(paste("Estimated tau:", tau_estimate))
print(paste("Estimated eta_0:", eta_0_estimate))

# Create a sequence of time points for plotting
time_points <- seq(0, 10, by = 0.1)

# Calculate true hazard rates for x = 0
true_hazard_control <- (cos(time_points * 3) + 1) / 2 * exp(log(lambda_0) + 0 * tau)
true_hazard_treated <- (cos(time_points * 3) + 1) / 2 * exp(log(lambda_0) + tau)

# Calculate estimated hazard rates for x = 0
estimated_hazard_control <- 
  (cos(time_points * 3) + 1) / 2 * exp(eta_0_estimate)
estimated_hazard_treated <- 
  (cos(time_points * 3) + 1) / 2 * exp(eta_0_estimate + tau_estimate)

# Create a data frame for plotting
plot_data <- data.frame(
  time = rep(time_points, 4),
  hazard = c(true_hazard_control, true_hazard_treated, estimated_hazard_control, estimated_hazard_treated),
  group = factor(rep(c("True Control", "True Treated", "Estimated Control", "Estimated Treated"), each = length(time_points)))
)

# Plot the hazard curves
ggplot(plot_data, aes(x = time, y = hazard, color = group, linetype = group)) +
  geom_line(size = 1) +
  labs(title = "True vs Estimated Hazard Curves for x = 0",
       x = "Time",
       y = "Hazard Rate") +
  theme_minimal()
