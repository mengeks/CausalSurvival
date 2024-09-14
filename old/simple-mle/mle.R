setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")
source("mle-utils.R")

# A simulation to show the right coverage
lambda_0 <- 1/2; lambda_1 <- 1/1
one_sample <-
  gen_one_sample(n = 1000,
                 lambda = c(lambda_0, lambda_1),
                 lambda_c = 1/3,
                 lambda_a = 1/1)

U <- one_sample$U;
Delta <- one_sample$Delta
A <- one_sample$A

r_0 <- sum( (U < A) * Delta)
W_0 <- sum((U < A) * U + (U >= A) * A )

hat_lambda_0 <- r_0 / W_0

r_1 <- sum( (U >= A) * Delta)
W_1 <- sum( (U >= A) * (U - A))

hat_lambda_1 <- r_1 / W_1


## Test coverage for null
R <- 100

n = 100; 
# Null
lambda_0 <- lambda_1 <-  1/1
## Alternative
# lambda_0 <- 1/2
# lambda_1 <-  1/1
lambda_c <- 1/3
lambda_a <- 1/1 

alpha <- 2
give_p_value <- function(beta, R){
  ests_lambda_0 <-
    ests_lambda_1 <- 
    rejects <- 
    Zs <- numeric(R)
  for (r in 1:R){
    
    # r <- 1
    print(r)
    set.seed(r * 100 + 23)
    X <- rnorm(n, mean=0,sd=1)
    
    
    source("mle-utils.R")
    one_sample <-
      gen_one_sample(n = n,
                     lambda = data.frame(lambda_0 = lambda_0 * exp(beta*X),
                                         lambda_1 = lambda_1 * exp(beta*X)),
                     lambda_c = lambda_c,
                     lambda_a = lambda_a * exp(alpha*X))
    
    
    U <- one_sample$U;
    Delta <- one_sample$Delta
    A <- one_sample$A
    
    r_0 <- sum( (U < A) * Delta)
    W_0 <- sum((U < A) * U + (U >= A) * A )
    
    hat_lambda_0 <- r_0 / W_0
    
    r_1 <- sum( (U >= A) * Delta)
    W_1 <- sum( (U >= A) * (U - A))
    
    hat_lambda_1 <- r_1/W_1
    
    ests_lambda_0[r] <- hat_lambda_0
    ests_lambda_1[r] <- hat_lambda_1
    
    Zs[r] <- 
      Z <- (log(hat_lambda_1) - log(hat_lambda_0)) /
      sqrt(1/r_0 + 1/r_1)
    
    rejects[r] <-
      abs(Z) > 1.96
  }
  return(
    list(
      ests_lambda_0 = (ests_lambda_0),
      ests_lambda_1 = (ests_lambda_1),
      Zs = (Zs),
      rejects = (rejects)
    )
  )
}

sim_RCT <- give_p_value(beta=0, R=100)
sim_Confounding <- give_p_value(beta=1.5, R=100)

saveRDS(sim_RCT, 
        file="~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/sim_RCT.rds")
saveRDS(sim_Confounding, 
        file="~/Dropbox (Harvard University)/Xiang_Iav/code/outputs/sim_Confounding.rds")


(point_est_RCT <- mean(sim_RCT$Zs))
(p_val_RCT <- 1 - mean(sim_RCT$rejects))
(point_est_Confounding <- mean(sim_Confounding$Zs))
(p_val_Confounding <- 1 - mean(sim_Confounding$rejects))

