library(tidyverse)
library(survival)
library(here)
source("R/datagen-helper.R")
library(testthat)
test_that("sigma function works for normal(0,1) input and plots its behavior", {
  sigma <- function(x) 2 / (1 + exp(-12 * (x - 0.5)))
  # Step 1: Generate 1000 samples from a normal(0,1) distribution
  set.seed(123)  # For reproducibility
  x <- rnorm(1000, mean = 0, sd = 1)
  
  # Step 2: Apply the sigma function to the samples
  sigma_values <- sigma(x)
  hist(sigma_values)
  
  x_vals <- seq(-3, 3, length.out = 100)  # Values from -3 to 3
  y_vals <- sigma(x_vals)
  
  # Create the plot using ggplot2
  plot_data <- data.frame(x = x_vals, sigma = y_vals)
  
  p <- ggplot(plot_data, aes(x = x, y = sigma)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Plot of Sigma Function", x = "x", y = "sigma(x)") +
    theme_minimal()
  
  print(p)
})


test_that("generate_simulated_data generates good data for eta_type  = 10-dim-non-linear", {
  n <- 200
  eta_type <- "10-dim-non-linear"
  lambda_C = 0.1
  seed_value = 123
  
  source("R/datagen-helper.R")
  sim_non_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "non-linear",
    seed_value = params$seed_value,
    verbose = 0
  )
  
  # test: CATE better have a range of values
  hist(sim_non_linear$CATE)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_non_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_non_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the range of non-censored time should not be too small
  hist(sim_non_linear$T)
  expect_true(max(sim_non_linear$T) >= 9)
  
  
  source("R/datagen-helper.R")
  sim_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = seed_value,
    verbose = 0
  )
  # test: the range of non-censored time should not be too small
  hist(sim_linear$T)
  expect_true(max(sim_linear$T) >= 9)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_linear$CATE)
  
  sim_zero <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "zero",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_zero$T)
  expect_true(max(sim_zero$T) >= 9)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_zero %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_zero %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_zero$CATE)
  
  sim_constant <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "constant",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_constant$T)
  expect_true(max(sim_constant$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_constant %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_constant %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_constant$CATE)
  
  sim_ReLU <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "ReLU",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_ReLU$T)
  expect_true(max(sim_ReLU$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_ReLU %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_ReLU %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_ReLU$CATE)
})


test_that("generate_simulated_data generates good data for eta_type  = 10-dim-non-linear", {
  n <- 200
  eta_type <- "10-dim-linear"
  lambda_C = 0.1
  seed_value = 123
  
  source("R/datagen-helper.R")
  sim_zero <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "zero",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_zero$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_zero %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_zero %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  expect_true(proportion_Delta > 0.7)
  
  # test: CATE better have a range of values
  hist(sim_zero$CATE)
  
  sim_non_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "non-linear",
    seed_value = params$seed_value,
    verbose = 0
  )
  
  
  # test: the range of non-censored time should not be too small
  hist(sim_non_linear$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_non_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_non_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_non_linear$CATE)
  
  source("R/datagen-helper.R")
  sim_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = seed_value,
    verbose = 0
  )
  # test: the range of non-censored time should not be too small
  hist(sim_linear$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_linear$CATE)
  
  
  source("R/datagen-helper.R")
  sim_constant <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "constant",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_constant$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_constant %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_constant %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_constant$CATE)
  
  
  sim_ReLU <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "ReLU",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_ReLU$T)
  expect_true(max(sim_ReLU$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_ReLU %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_ReLU %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_ReLU$CATE)
})


test_that("generate_simulated_data generates good data for eta_type  = 10-dim-non-linear", {
  n <- 200
  eta_type <- "non-linear"
  lambda_C = 0.1
  seed_value = 123
  
  source("R/datagen-helper.R")
  sim_zero <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "zero",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_zero$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_zero %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_zero %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  expect_true(proportion_Delta > 0.7)
  
  # test: CATE better have a range of values
  hist(sim_zero$CATE)
  
  sim_non_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "non-linear",
    seed_value = params$seed_value,
    verbose = 0
  )
  
  
  # test: the range of non-censored time should not be too small
  hist(sim_non_linear$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_non_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_non_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_non_linear$CATE)
  
  source("R/datagen-helper.R")
  sim_linear <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "linear",
    seed_value = seed_value,
    verbose = 0
  )
  # test: the range of non-censored time should not be too small
  hist(sim_linear$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_linear %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_linear %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_linear$CATE)
  
  
  source("R/datagen-helper.R")
  sim_constant <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "constant",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_constant$T)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_constant %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_constant %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_constant$CATE)
  
  
  sim_ReLU <- generate_simulated_data(
    n, 
    lambda_C = lambda_C,
    eta_type = eta_type,
    CATE_type = "ReLU",
    seed_value = seed_value,
    verbose = 0
  )
  
  # test: the range of non-censored time should not be too small
  hist(sim_ReLU$T)
  expect_true(max(sim_ReLU$T) >= 8)
  
  # test: the proportion of treated should be 
  #   between 0.2 and 0.8, ideally be close to 0.5
  proportion_W <- sim_ReLU %>%
    mutate(W = A < U) %>%
    summarise(proportion_W = mean(W))
  print(proportion_W)
  expect_true(proportion_W > 0.2 & proportion_W < 0.8)
  
  # test: the proportion of non-censored should be greater than 0.7
  proportion_Delta <- sim_ReLU %>%
    summarise(proportion_Delta = mean(Delta))
  print(proportion_Delta)
  
  # test: CATE better have a range of values
  hist(sim_ReLU$CATE)
})
