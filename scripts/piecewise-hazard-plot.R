library(ggplot2)
library(here)

# Define the constants
lambda_0 <- 1/2
a <- 1.5
lambda_1 <- 1/1.5
tend <- 2.5

######
## Plot 
######
time_f0 <- seq(0, a, length.out = 100)
f0 <- dexp(time_f0, rate = lambda_0) 

time_f1 <- seq(a, tend, length.out = 100) # Start f1 from a
f1 <- dexp(time_f1, rate = lambda_1) * (1 - pexp(a, rate = lambda_0)) / (1 - pexp(a, rate = lambda_1))

# Create data frame for plotting
df_density_f0 <- data.frame(time = time_f0, f0 = f0)
df_density_f1 <- data.frame(time = time_f1, f1 = f1)

# Plot using ggplot2
p <- ggplot() +
  geom_line(data = df_density_f0, aes(x = time, y = f0), color = "black", size = 1.5) + # f0 baseline
  geom_line(data = df_density_f1, aes(x = time, y = f1), color = "blue", size = 1.5) + # f1 treated only for time >= a
  geom_vline(xintercept = a, linetype = "dotted", size = 1.5) + # Vertical line for "a"
  annotate("text", x = a/2, y = max(f0), label = expression(f[0](t)), color = "black", size = 7) + # Label for f0
  annotate("text", x = (a + tend) / 2, y = max(f1), label = expression(f[1](t)), color = "blue", size = 7) + # Label for f1
  labs(x = "t", y = "Density") +
  theme_minimal(base_size = 30) + # Increase base size
  theme(panel.grid = element_blank()) # Remove grid background
p
# Save the plot
ggsave(filename = here("figures/f-tx-f-co.png"),
       plot = p)


#### 
## Plotting the hazards
######
plot_hazard <- function(a){
  
  x <- seq(0, tend, by = 0.01)
  y <- ifelse(x <= a, lambda_0, lambda_1)
  
  # Create a data frame for plotting
  df_hazard <- data.frame(time = x, hazard = y)
  
  # Plot using ggplot2
  p <- ggplot(df_hazard, aes(x = time, y = hazard)) +
    geom_line(aes(color = ifelse(time <= a, "black", "blue")), size = 1.5) + # Change color before and after 'a'
    scale_color_manual(values = c("black" = "black", "blue" = "blue")) + # Set colors
    geom_vline(xintercept = a, linetype = "dotted", size = 1.5) + # Vertical line at 'a'
    annotate("text", x = a/2, y = lambda_0 + 0.05, label = expression(h[0](x)), color = "black", size = 10) + # Label for h0 (doubled text size)
    annotate("text", x = (a + tend) / 2, y = lambda_1 + 0.05, label = expression(h[1](x)), color = "blue", size = 10) + # Label for h1 (doubled text size)
    labs(x = "t", y = "Hazard value") +
    theme_minimal(base_size = 30) + # Double base size for axis labels and titles
    theme(panel.grid = element_blank(), # Remove grid background
          legend.position = "none") # Remove the color legend
  p
}

plot_hazard(a = 1)
ggsave(filename = 
         here("figures/hazard-viz-a-1.png"), width = 8, height = 5)
plot_hazard(a = 2)
ggsave(filename = 
         here("figures/hazard-viz-a-2.png"), 
       width = 8, height = 5)



