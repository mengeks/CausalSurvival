setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")
source("mle-utils.R")
library(tidyverse)
## Test coverage for null
n = 100; 
# lambda_1 > lambda_0 means the treat
lambda_0 <- 1
lambda_1 <- 2 
tau <- log(lambda_1) - log(lambda_0)
# Set lambda_c to be very small such that the
#   mean is very large, and we can ignore censoring
lambda_c <- 1/2
# Set lambda_a to be large such that its mean is small
#  Better to have lambda_a larger than 
lambda_a <- 1

beta_X_sq <- 0

give_p_value <- function(beta, R){
  ests_lambda_0 <-
    ests_lambda_1 <- 
    rejects <- tau_ses <- tau_estimates <-
    Zs <- numeric(R)
  # beta = 1.5; alpha = 2
  for (r in 1:R){
    # r <- 31
    print(r)
    set.seed(r * 100 + 23)
    X <- rnorm(n, mean=0,sd=1)
    
    source("mle-utils.R")
    one_sample <-
      gen_one_sample(
        n = n,
        lambda = data.frame(
          lambda_0 = lambda_0 * exp(beta*X),
          lambda_1 = lambda_1 * 
            exp(beta*X + beta_X_sq * X^2)
        ),
        lambda_c = lambda_c,
        lambda_a = lambda_a * exp(alpha*X)
      )
    
    
    U <- one_sample$U;
    Delta <- one_sample$Delta
    A <- one_sample$A
    
    one_sample_tibble <- tibble(
      U = U, 
      Delta = Delta, 
      A = A,
      X = X
    ) %>% mutate(id = 1:n())
    
    
    # Create pseudo_dataset
    # Initialize an empty tibble for the pseudo dataset
    pseudo_dataset <- 
      tibble(
        t = numeric(), 
        d = numeric(), 
        W = numeric()
      )
    
    # Iterate over each row in the one_sample_tibble
    for (i in 1:nrow(one_sample_tibble)) {
      # i <- 1
      U_i <- one_sample_tibble$U[i]
      Delta_i <- one_sample_tibble$Delta[i]
      A_i <- one_sample_tibble$A[i]
      id_i <- one_sample_tibble$id[i]
      X_i <- one_sample_tibble$X[i]
      if (U_i <= A_i && A_i <= Inf) {
        # Create one pseudo observation
        new_rows <- tibble(
          t = U_i, 
          d = Delta_i, 
          W = 0
          )
      } else if (A_i < U_i) {
        # Create two pseudo observations
        new_rows <- tibble(
          t = c(A_i, U_i-A_i),
          d = c(0, Delta_i),
          W = c(0, 1)
        )
      }
      new_rows <- new_rows %>%
        mutate(id = id_i,
               X = X_i)
      pseudo_dataset <- 
        bind_rows(pseudo_dataset, 
                  new_rows)
    }
    # Remove very rare case that t might be zero
    pseudo_dataset <- 
      pseudo_dataset %>% 
      filter(t > 0)
    pseudo_dataset <- 
      mutate(pseudo_dataset, os=log(t))
    mpwexp <- 
      glm(d ~ X + W, 
          offset=os, 
          family=poisson, 
          data=pseudo_dataset
          )
    mpwexp_summary <- summary(mpwexp)
    tau_estimates[r] <- 
      tau_estimate <- 
      mpwexp_summary$coefficients[3,1]
    tau_ses[r] <- 
      tau_se <- 
      mpwexp_summary$coefficients[3,2]
    
    Zs[r] <- 
      Z <- (tau_estimate - tau) /
      tau_se
    
    rejects[r] <-
      abs(Z) > 1.96
  }
  return(
    list(
      tau_estimates = tau_estimates,
      tau_ses = tau_ses,
      Zs = Zs,
      rejects = rejects,
      model_summary = mpwexp_summary
    )
  )
}

# Set alpha to 0, representing RCT
alpha <- 0
tictoc::tic()
sim_RCT <- give_p_value(beta=1.5, R=100)
tictoc::toc()

path_for_sim_RCT <- "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/panel-hazard-regression/sim_RCT.rds"
saveRDS(sim_RCT, 
        file=path_for_sim_RCT)
sim_RCT <- readRDS(file = path_for_sim_RCT)
(point_est_RCT <- mean(sim_RCT$Zs))
(p_val_RCT <- 1 - mean(sim_RCT$rejects))
hist(sim_RCT$tau_estimates)
abline(v=tau,col="red")

# Set alpha to 2, representing confounding
alpha <- 2
beta_X_sq <- 1
tictoc::tic()
sim_confounding <- give_p_value(beta=1.5, R=100)
tictoc::toc()

path_for_sim_confounding <- 
  "~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/panel-hazard-regression/sim_confounding.rds"
saveRDS(sim_confounding, 
        file=path_for_sim_confounding)
sim_confounding <- readRDS(file=path_for_sim_confounding)
(point_est_confounding <- mean(sim_confounding$Zs))
(p_val_confounding <- 1 - mean(sim_confounding$rejects))
# p-value is 0.97.

hist(sim_confounding$tau_estimates)
abline(v=tau,col="red")
# It's hard to see whether the confounding kicks in
#   with good p-value and non-normal historgram
#   due to small run size (R=100)
# I should increasing run size then see whether 
#   the confounding kicks in or not, while 
#   researching how to kick in confounding


