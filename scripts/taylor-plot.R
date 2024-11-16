library(ggplot2)
library(gridExtra)
library(dplyr)

# Function definitions
f <- function(h, h0 = 2) {
(h - h0)^2 + (h - h0)
}

g <- function(h, h0 = 2) {
  (h - h0)^2
}

# Generate data
h_seq <- seq(0, 4, by = 0.01)
h0 <- 2

plot_data <- data.frame(
  h = rep(h_seq, 2),
  value = c(f(h_seq), g(h_seq)),
  # type = rep(c("f(h)", "g(h)"), each = length(h_seq))
  type = rep(c("vanilla", "robust"), each = length(h_seq))
)

# Main comparison plot
p1 <- ggplot(plot_data, aes(x = h, y = value, color = type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = h0, 
             linetype = "dashed", 
             color = "gray") +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "hat h0", y = "Function value", 
       title = "Best Parabola Approx. of Likelihood") +
  theme_minimal() +
  annotate("text", x = h0, y = -0.5, label = "h0") +
  coord_cartesian(ylim = c(-0.5, 5))

# Zoomed view near h0
p2 <- ggplot(plot_data %>% filter(h >= 1.75, h <= 2.25), 
             aes(x = h, y = value, color = type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = h0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "hat h0", y = "Function value", 
       title = "Zoomed view near h0") +
  theme_minimal() +
  annotate("text", x = h0, y = 0, label = "h₀")

# Convergence rate plot
n_iter <- 100
h_sequence <- h0 + exp(-seq(0, 5, length.out = n_iter))
convergence_data <- data.frame(
  iteration = rep(1:n_iter, 2),
  error = c(
    abs(f(h_sequence) - f(h0)),
    abs(g(h_sequence) - g(h0))
  ),
  type = rep(c("f(h)", "g(h)"), each = n_iter)
)

p3 <- ggplot(convergence_data, aes(x = iteration, y = log(error), color = type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Iterations", y = "Log error", 
       title = "Convergence rates") +
  theme_minimal()

# Combine plots
# p <- grid.arrange(p1, p2, ncol =2)
# ggsave(file = "figures/taylor-plot.png",plot= p )
p <- arrangeGrob(p1, p2, ncol=2) 
ggsave("figures/taylor-plot.png", p, width = 8, height = 5)


# library(ggplot2)
# library(gridExtra)
# library(dplyr)
# 
# # Define the functions and their derivatives
# # f(h) = (h - h0)^2 + (h - h0), a quadratic function with linear term
# # g(h) = (h - h0)^2, a pure quadratic function
# # h0 = 2 (the minimum point)
# 
# # Function definitions
# f <- function(h, h0 = 2) {
#   (h - h0)^3 + (h - h0)^2 + (h - h0)
# }
# 
# g <- function(h, h0 = 2) {
#   0.5 * (h - h0)^3 + (h - h0)^2
# }
# 
# # Derivatives at h0
# f_prime_h0 <- 1  # f'(h0) = 1
# f_double_prime_h0 <- 2  # f''(h0) = 2
# g_prime_h0 <- 0  # g'(h0) = 0
# g_double_prime_h0 <- 2  # g''(h0) = 2
# 
# # Taylor approximations
# f_taylor <- function(h, h0 = 2) {
#   f(h0) + f_prime_h0 * (h - h0) + (f_double_prime_h0/2) * (h - h0)^2
# }
# 
# g_taylor <- function(h, h0 = 2) {
#   g(h0) + (g_double_prime_h0/2) * (h - h0)^2
# }
# 
# # Generate data
# h_seq <- seq(0, 4, by = 0.01)
# h0 <- 2
# 
# plot_data <- data.frame(
#   h = rep(h_seq, 4),
#   value = c(
#     f(h_seq),
#     f_taylor(h_seq),
#     g(h_seq),
#     g_taylor(h_seq)
#   ),
#   type = rep(c("f(h)", "f Taylor", "g(h)", "g Taylor"), each = length(h_seq))
# )

# # Create plots
# # Main comparison plot
# p1 <- ggplot(plot_data, 
#              aes(x = h, y = value, color = type, linetype = type)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = h0, linetype = "dashed", color = "gray") +
#   scale_color_manual(values = c("red", "pink", "blue", "lightblue")) +
#   scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
#   labs(x = "h", y = "Function value", 
#        title = "Functions and their Taylor approximations") +
#   theme_minimal() +
#   annotate("text", x = h0, y = -0.5, label = "h₀") +
#   coord_cartesian(ylim = c(-0.5, 5))
# 
# # Zoomed view near h0
# p2 <- ggplot(plot_data %>% filter(h >= 1.5, h <= 2.5), 
#              aes(x = h, y = value, color = type, linetype = type)) +
#   geom_line(size = 1) +
#   geom_vline(xintercept = h0, linetype = "dashed", color = "gray") +
#   scale_color_manual(values = c("red", "pink", "blue", "lightblue")) +
#   scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
#   labs(x = "h", y = "Function value", 
#        title = "Zoomed view near h₀") +
#   theme_minimal() +
#   annotate("text", x = h0, y = 0, label = "h₀")
# 
# # Convergence rate plot
# n_iter <- 100
# h_sequence <- h0 + exp(-seq(0, 5, length.out = n_iter))
# convergence_data <- data.frame(
#   iteration = rep(1:n_iter, 2),
#   error = c(
#     abs(f(h_sequence) - f(h0)),
#     abs(g(h_sequence) - g(h0))
#   ),
#   type = rep(c("f(h)", "g(h)"), each = n_iter)
# )
# 
# p3 <- ggplot(convergence_data, 
#              aes(x = iteration, y = log(error), color = type)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("red", "blue")) +
#   labs(x = "Iterations", y = "Log error", 
#        title = "Convergence rates") +
#   theme_minimal()
# 
# # Combine plots
# grid.arrange(p1, p2, p3, ncol = 3)

