library(ggplot2)

# Define the constants
lambda_0 <- 1/2
a <- 1.5
lambda_1 <- 1/1.5
tend <- 2.5

# Define an increasing function for lambda(t)
lambda_t <- function(t) {
  0.5 + t  # Simple increasing function for illustration
}

# Define a function to plot lambda(t)
plot_lambda <- function() {
  x <- seq(0, tend, by = 0.01)
  y <- lambda_t(x)
  
  # Create a data frame for plotting
  df_lambda <- data.frame(time = x, lambda = y)
  
  # Plot using ggplot2
  p <- ggplot(df_lambda, aes(x = time, y = lambda)) +
    geom_line(color = "red", size = 1.5) +
    labs(x = "t", y = expression(lambda(t))) +
    theme_minimal(base_size = 30) +
    theme(panel.grid = element_blank())
  p
}

# Define a function to plot lambda(t) * hazard(a = 1)
plot_lambda_hazard <- function(a) {
  x <- seq(0, tend, by = 0.01)
  hazard <- ifelse(x <= a, lambda_0, lambda_1)  # Hazard function
  lambda_values <- lambda_t(x)
  product <- lambda_values * hazard
  
  # Create a color indicator: "black" before a, "mix" after a
  color_indicator <- ifelse(x <= a, "black", "mix")
  
  # Create a data frame for plotting
  df_product <- data.frame(time = x, product = product, color = color_indicator)
  
  # Plot using ggplot2
  p <- ggplot(df_product, aes(x = time, y = product, color = color)) +
    geom_line(size = 1.5) +
    geom_vline(xintercept = a, linetype = "dotted", size = 1.5) + # Vertical line at 'a'
    scale_color_manual(
      values = c("black" = "black", "mix" = "blue"), # Change "mix" to blue (can customize further)
      guide = "none" # Remove legend
    ) +
    geom_line(data = subset(df_product, time > a), 
              aes(x = time, y = product), color = "red", size=1.5)+ 
    theme_minimal(base_size = 30) +
    theme(panel.grid = element_blank())
  return (p)
}


# Plot the increasing lambda(t)
plot_lambda()
ggsave(filename =
         here("figures/hazard-lambda.png"), width = 8, height = 5)

plot_lambda_hazard(a = 1)
ggsave(filename =
         here("figures/hazard-cox-a-1.png"), width = 8, height = 5)

# ggsave(filename = 
#          here("figures/hazard-viz-a-2.png"), 
#        width = 8, height = 5)




# plot \lambda 

