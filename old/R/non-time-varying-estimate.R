non_time_varying_estimate <- 
  function(simulated_data,
           causal=F,
           light_censoring=F,
           outcome_type = "linear"){
    if (outcome_type == "linear"){
      outcome_formula <- 
        as.formula(Surv(U,Delta) ~ 
                     W + X)
    }else if (outcome_type == "non-linear"){
      outcome_formula <- 
        as.formula(Surv(U,Delta) ~ 
                     W + X + I(X^2))
    }
    
    if (causal == F){
      model <- 
        coxph(formula = 
                outcome_formula,
              data = simulated_data, 
              ties = "breslow")
      
      return(model)
    }else{
      
      folds <- sample(1:2, size = n, replace = TRUE)
      fold1 <- simulated_data[folds == 1, ]
      fold2 <- simulated_data[folds == 2, ]
      
      DINA_single_split <- function(fold1, fold2){
        ## Implement DINA:
        # We split data into two folds
        # On fold one, we 
        #   - estimate p_delta(W,X) = P(Delta = 1| W, X) using logistic regression
        #   - estimate prop_score(X) = P(W = 1 | X) using logistic regression
        #   - Fit the Surv(U,Delta) ~ W + X model. The coefficient for  
        #     W is tau_estimate, for X is beta_estimate
        # On fold two, for each row of data, we calculate 
        #   - eta_0(X) = beta_estimate * X; eta_1(X) = beta_estimate * X + tau_estimate
        #   - a(X) = p_delta(W=1,X) * prop_score(X) / 
        #             (p_delta(W=1,X) * prop_score(X) +  
        #              p_delta(W=0,X) * (1 - prop_score(X)))
        #   - nu(X) = a(X) * eta_1(X) + (1 - a(X)) * eta_0(X)
        #   - Fit the Surv(U,Delta) ~ (W - a(X)), and set the offset to nu(X)
        # 2. Estimate prop_score(X) using logistic regression
        prop_score_model <- 
          glm(W ~ X, 
              data = fold1, 
              family = binomial)
        prop_score <-
          predict(prop_score_model,
                  fold2,
                  type = "response")
        # expit <- function(x) 1 / (1 + exp(-x))  
        # prop_score <- 
        #   expit(alpha * fold2$X)
        
        # 3. Fit the Surv(U, Delta) ~ W + X model
        cox_model <- 
          coxph(outcome_formula, 
                data = fold1, 
                ties = "breslow")
        
        tau_estimate <- cox_model$coefficients['W']
        
        # Step 3: Calculate adjusted parameters on fold 2
        if (outcome_type == "linear"){
          beta_estimate <- cox_model$coefficients['X']
          eta_0 <- 
            beta_estimate * fold2$X
        }else if (outcome_type == "non-linear"){
          beta_0_estimate <- cox_model$coefficients['X']
          beta_1_estimate <- cox_model$coefficients['I(X^2)']
          eta_0 <- 
            beta_0_estimate * fold2$X + beta_1_estimate * (fold2$X)^2
        }
        
        eta_1 <- eta_0 + 
          tau_estimate
        
        # 2. Calculate a(X)
        if (light_censoring){
          a_X <- prop_score
        }else{
          # 1. Estimate p_delta(W, X) using logistic regression
          p_delta_model <- 
            glm(Delta ~ W + X, 
                data = fold1, 
                family = binomial)
          p_delta <- predict(p_delta_model, fold2, type = "response")
          a_X <- (p_delta * prop_score) / (p_delta * prop_score + (1 - p_delta) * (1 - prop_score))
        }
        
        # 3. Calculate nu(X)
        nu_X <- 
          a_X * eta_1 + (1 - a_X) * eta_0
        
        fold2$a_X <- a_X
        fold2$nu_X <- nu_X
        return(fold2)
      }
      
      result_fold2 <- DINA_single_split(fold1, fold2)
      result_fold1 <- DINA_single_split(fold2, fold1)
      # 4. Fit the final model
      final_model <- 
        coxph(Surv(U, Delta) ~ 
                I(W - a_X) + offset(nu_X), 
              data = rbind(result_fold2,
                           result_fold1), 
              ties = "breslow")
      return(final_model)
      # # Print the summary of the final model
      # summary(final_model)
    }
  }
