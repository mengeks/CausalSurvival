setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")
library(tidyverse)
library(survival)
source("cox-utils.R")
source("datagen-helper.R")

# Parameters
n <- 100  # Number of observations
alpha <- 1
lambda_C <- 0.1
lambda_0 <- 1
tau <- 1
beta <- 1.5
beta_2 <- 1.5  # Coefficient for the non-linear term
R <- 100

## Next make time-varying DINA
is_time_varying <- T; is_linear <- F
set.seed(33)
n <- 1000
simulated_data <- 
  generate_simulated_data(
    is_time_varying = is_time_varying, 
    is_linear = is_linear
  )
pseudo_dataset <- 
  create_pseudo_dataset(
    survival_data = simulated_data)


model <- 
  coxph(formula = Surv(tstart, tstop, Delta) ~ 
          W + X,
        data = pseudo_dataset, 
        ties = "breslow")
model_summary <- 
  summary(model)
tau_estimate <- 
  model_summary$coefficients[1,1]
tau_estimate # 0.79


## Implement DINA:
# We split data into two folds
# On fold one, we 
#   - estimate p_delta(W,X) = P(Delta = 1| W, X) using logistic regression
#   - estimate hazard of A as a function of X. The correct hazard is e^{\alpha X_i}
#     Denote this as hazard_adapt_date(X)
#   - Fit Surv(tstart, tstop, d) ~ W + X model. The coefficient for  
#     W is tau_estimate, for X is beta_estimate
# On fold two, for each row of data, we calculate 
#   - eta_0(X) = beta_estimate * X; eta_1(X) = beta_estimate * X + tau_estimate
#   - a(X) = p_delta(W=1,X) * hazard_adapt_date(X) / 
#             (p_delta(W=1,X) * hazard_adapt_date(X) +  
#              p_delta(W=0,X) * (1 - hazard_adapt_date(X)))
#   - nu(X) = a(X) * eta_1(X) + (1 - a(X)) * eta_0(X)
#   - Fit the Surv(tstart, tstop, d) ~ (W - a(X)), and set the offset to nu(X)
# Set seed and generate simulated data

simulated_data <-
  simulated_data %>% 
  mutate(U_A = pmin(A,U),
         Delta_A = A <= U)


obtain_nuisance_data <- 
  function(nuisance_fold, 
           nuisance_simulated_fold, 
           causal_fold, 
           causal_simulated_fold) {
    # nuisance_fold <- fold1
    # causal_fold <- fold2
    # nuisance_simulated_fold <- simulated_fold1
    # causal_simulated_fold <- simulated_fold2
    
  # Estimate p_delta(W,X) using logistic regression
  p_delta_model <- 
    glm(Delta ~ W + X, 
        data = nuisance_fold, 
        family = binomial)
  p_delta <- 
    predict(p_delta_model, 
            causal_fold, 
            type = "response")
  
  hazard_model <- 
    coxph(Surv(U_A, Delta_A) ~ X, 
          data = nuisance_simulated_fold, 
          ties = "breslow")
  alpha_estimate <- 
    hazard_model$coefficients
  
  # # Pass the survival_prob_UA to causal_simulated_fold
  # causal_simulated_fold$survival_prob_UA <- survival_prob_UA
  
  # # Pass the survival_prob_UA to causal_fold by matching the id
  # causal_fold$survival_prob_UA <- 
  #   survival_prob_UA[match(causal_fold$id, 
  #                          causal_simulated_fold$id)]
  
  # survival_prob_UA <- sapply(1:nrow(causal_simulated_fold), function(i) {
  #   surv_fit <- survfit(hazard_model, newdata = causal_simulated_fold[i, ])
  #   summary(surv_fit, times = causal_simulated_fold$U_A[i])$surv
  # })
  # 
  
  # Estimate tau_estimate and beta_estimate using coxph
  cox_model <- 
    coxph(Surv(tstart, tstop, Delta) ~ W + X, 
          data = nuisance_fold, 
          ties = "breslow")
  beta_estimate <- cox_model$coefficients['X']
  tau_estimate <- cox_model$coefficients['W']
  
  causal_fold <- 
    causal_fold %>% 
    mutate(eta_0 = beta_estimate * X) %>%
    mutate(eta_1 = eta_0 + tau_estimate)
  causal_fold$p_delta <- p_delta
  
  granular_cut_points <-
    unique(causal_fold$tstop)
  
  split_within_intervals <- function(row, cut_points) {
    original_tstart <- row[["tstart"]]
    original_tstop <- row[["tstop"]]
    
    # Filter cut points to be within the original tstart and tstop
    valid_cuts <- cut_points[cut_points > original_tstart & cut_points < original_tstop]
    
    # Add the original tstart and tstop to the list of cut points
    final_cuts <- sort(c(original_tstart, valid_cuts, original_tstop))
    
    # Create new rows for each interval
    new_intervals <- data.frame(
      tstart = head(final_cuts, -1),
      tstop = tail(final_cuts, -1),
      Delta = row[["Delta"]],
      W = row[["W"]],
      X = row[["X"]],
      id = row[["id"]],
      eta_0 = row[["eta_0"]],
      eta_1 = row[["eta_1"]],
      p_delta = row[["p_delta"]]
    )
    
    return(new_intervals)
  }
  
  causal_fold_split <- 
    map_dfr(seq_len(nrow(causal_fold)), function(i) {
      split_within_intervals(causal_fold[i, ], granular_cut_points)
    })

  calculate_eX <- 
    function(alpha_estimate, X, t){
      hazard_adapt_date <- 
        exp(alpha_estimate * X)
      trt_prob <- 
        1 - exp(-t * hazard_adapt_date)
      return(trt_prob)
    }
  
  causal_fold_split <-
    causal_fold_split %>%
    mutate(e_X = calculate_eX(
                  alpha_estimate, X, tstop
                  )
           ) %>%
  mutate(
    a_X = (p_delta * e_X) / 
      (p_delta * e_X + (1 - p_delta) * (1 - e_X)),
    nu_X = a_X * eta_1 + (1 - a_X) * eta_0
  )
  
  
  
  return(causal_fold_split)
}

# Split data into two folds using ids from simulated_data
folds <- sample(1:2, size = nrow(simulated_data), replace = TRUE)
simulated_fold1 <- simulated_data[folds == 1, ]
simulated_fold2 <- simulated_data[folds == 2, ]

# Use the ids from simulated_fold1 and simulated_fold2 to subset pseudo_dataset
fold1 <- 
  pseudo_dataset[pseudo_dataset$id %in% simulated_fold1$id, ]
fold2 <- 
  pseudo_dataset[pseudo_dataset$id %in% simulated_fold2$id, ]





# Calculate a_X and nu_X for fold1 and fold2
results_fold1 <- 
  obtain_nuisance_data(
    fold1, simulated_fold1,
    fold2, simulated_fold2)
results_fold2 <- 
  obtain_nuisance_data(
    fold2, simulated_fold2,
    fold1, simulated_fold1)

combined_data <- rbind(
  results_fold1,
  results_fold2
  # transform(fold1, a_X = results_fold1$a_X, nu_X = results_fold1$nu_X),
  # transform(fold2, a_X = results_fold2$a_X, nu_X = results_fold2$nu_X)
)

# Fit the final model on the combined data
final_model <- coxph(
  Surv(tstart, tstop, Delta) ~ 
    I(W - a_X) + offset(nu_X), 
      data = combined_data, ties = "breslow")
# What is tricky here is that nu_X, a_X depends on 
#     time, too. For each tstart, tstop, 
#     I am able to calculate value of nu_X, a_X
#     at each time. 
# I know inside coxph, a likelihood function
#   is being constructed and optimized.
# I know the likelihood function is summed over
#   a subset of tstop
# How should I proceed


# Extract the causal estimate for tau
tau_estimate_causal <- summary(final_model)$coefficients[1, 1]
tau_estimate_causal

