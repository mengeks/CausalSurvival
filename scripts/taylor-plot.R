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
       title = "Best Parabola Approx. of Influence Function") +
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
# p <- arrangeGrob(p2, p1, ncol=2) 
# ggsave("figures/taylor-plot.png", p, width = 8, height = 5)
ggsave("figures/taylor-plot-zoomed.png", p2, width = 4, height = 5)
ggsave("figures/taylor-plot-enlarged.png", p1, width = 4, height = 5)

