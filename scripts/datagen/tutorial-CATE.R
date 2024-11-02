library(tidyverse)
library(survival)
source("R/datagen-helper.R")

# EDA: illustration of the adapted and non-adapted times
# If you have
# True difference is 

n <- 200
eta_type <- "10-dim-non-linear"
lambda_C = 0.1
seed_value = 123
sim_constant <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "constant",
  seed_value = seed_value,
  linear_intercept = 2,
  linear_slope_multiplier = 0,
  verbose = 0
) %>%
  mutate(W = A < U)


proportion_W <- sim_constant %>%
  summarise(proportion_W = mean(W))
print(proportion_W)

means <- sim_constant %>%
  group_by(W) %>%
  summarize(mean_U = mean(U))


ggplot(aes(x = U), data = sim_constant) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mean_U), data = means) + 
  facet_wrap(~ W)


# mean(rexp(1000, rate = 1/2)) - mean(rexp(1000, rate = exp(2) * 1/2))


source("R/datagen-helper.R")
sim_linear_1 <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "linear",
  seed_value = seed_value,
  verbose = 0
)
hist(sim_linear_1$T)

source("R/datagen-helper.R")
sim_linear_2 <- generate_simulated_data(
  n, 
  lambda_C = lambda_C,
  eta_type = eta_type,
  CATE_type = "linear",
  seed_value = seed_value,
  verbose = 0
)
hist(sim_linear_2$T)
