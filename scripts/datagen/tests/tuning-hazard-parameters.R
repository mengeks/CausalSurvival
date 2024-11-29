library(tidyverse)
library(survival)
library(here)
source("R/datagen-helper.R")
library(testthat)

# Helper function to run visualization
create_diagnostic_plots <- function(sim_df) {
  par(mfrow = c(2, 2))
  hist(sim_df$T, main = "Distribution of Survival Times", xlab = "T")
  hist(sim_df$eta_0, main = "Distribution of Baseline Effects", xlab = "eta_0")
  hist(sim_df$HTE, main = "Distribution of HTEs", xlab = "HTE")
  plot(sim_df$A, sim_df$T, main = "Treatment vs Survival Time", 
       xlab = "Treatment", ylab = "Survival Time")
  par(mfrow = c(1, 1))
}


test_simulated_data <- function(eta_type, HTE_type="linear",tx_difficulty="simple") {
  test_that(sprintf("generate_simulated_data generates good data for eta_type = %s, HTE_type = %s", eta_type, HTE_type), {
    
    # Setup parameters
    n <- 300
    lambda_C <- 0.1
    seed_value <- 123
    
    source("R/datagen-helper.R")
    sim_df <- generate_simulated_data(
      n, 
      lambda_C = lambda_C,
      eta_type = eta_type,
      HTE_type = HTE_type,
      tx_difficulty = tx_difficulty,
      seed_value = seed_value,
      linear_intercept = 0,
      linear_slope_multiplier = 2.5,
      linear_HTE_multiplier = 1, 
      verbose = 0
    )
    
    # Optional visualization
    if (TRUE) {
      create_diagnostic_plots(sim_df)
    }
    
    # Test 1: Distribution of survival times (T)
    test_that("Survival times follow expected distribution", {
      expect_true(all(sim_df$T > 0))
      q75_T <- quantile(sim_df$T, 0.75)
      expect_true(q75_T > 0.01)
    })
    
    # Test 2: Baseline effects (eta_0)
    test_that("Baseline effects are reasonable", {
      eta_range <- range(sim_df$eta_0)
      expect_true(diff(eta_range) > 0)
      expect_true(sd(sim_df$eta_0) > 0)
      expect_true(abs(mean(sim_df$eta_0)) < 10)
    })
    
    # Test 3: Treatment assignment
    test_that("Treatment assignment is balanced", {
      proportion_W <- sim_df %>%
        mutate(W = A < U) %>%
        summarise(proportion_W = mean(W)) %>%
        pull(proportion_W)
      
      expect_true(proportion_W > 0.2 & proportion_W < 0.8)
      expect_true(abs(proportion_W - 0.5) < 0.2)
    })
    
    # Test 4: Censoring
    test_that("Censoring proportion is appropriate", {
      proportion_Delta <- mean(sim_df$Delta)
      expect_true(proportion_Delta > 0.7)
      expect_true(proportion_Delta < 0.95)
    })
    
    # Test 5: Heterogeneous Treatment Effects
    test_that("HTE values are appropriate", {
      expect_true(sd(sim_df$HTE) > 0)
      expect_true(abs(mean(sim_df$HTE)) < 10)
    })
  })
}


test_simulated_data(eta_type = "linear", HTE_type = "linear")
test_simulated_data(eta_type = "non-linear", HTE_type = "linear")

test_simulated_data(eta_type = "non-linear", HTE_type = "linear", tx_difficulty = "constant")
test_simulated_data(eta_type = "linear", HTE_type = "linear", tx_difficulty = "constant")

source("R/datagen-helper.R")
test_simulated_data(eta_type = "non-linear", HTE_type = "linear", tx_difficulty = "complex")
test_simulated_data(eta_type = "linear", HTE_type = "linear", tx_difficulty = "complex")


test_that("sigma function works for normal(0,1) input and plots its behavior", {
  sigma <- function(x) 2 / (1 + exp(-12 * (x - 0.5)))
  # Step 1: Generate 1000 samples from a normal(0,1) distribution
  set.seed(123)  # For reproducibility
  
  x_vals <- seq(-3, 3, length.out = 100)  # Values from -3 to 3
  y_vals <- sigma(x_vals)
  
  plot_data <- data.frame(x = x_vals, sigma = y_vals)
  
  library(ggplot2)
  p <- ggplot(plot_data, aes(x = x, y = sigma)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Plot of the non-linear Function", x = "x", y = "sigma(x)") +
    theme_minimal()
  
  print(p)
  ggsave(filename = here::here("figures", "xi-plot.png"), plot = p)
  
  x1 <- rnorm(1000, mean = 0, sd = 1)
  x2 <- rnorm(1000, mean = 0, sd = 1)
  
  # Step 2: Apply the sigma function to the samples
  sigma_values <- - 1 / 2 * sigma(x1) * sigma(x2)
  png(filename = here::here("figures", "xi-hist.png"))
  hist(sigma_values)
  dev.off()
})
