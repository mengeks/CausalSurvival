## DataGen
gen_one_sample <- function(n, 
                           lambda_0,
                           lambda_c = 1/3){
  
  T = rexp(n = n, rate = lambda_0)
  
  # mean(T) = 2, i.e., rate is indeed lambda
  # we want mean = 1 / lambda
  C = rexp(n = n, rate = lambda_c) # a specification of C
  U = pmin(T,C); Delta <- T <= C
  
  return(list(
    U=U,
    Delta = Delta
  ))
}

## Estimation
get_r_W <- function(U, Delta){
  r = sum(Delta)
  W = sum(U)
  return(list(r = r,
              W = W))
}

get_est <- function(U, Delta){
  r_W <- get_r_W(U, Delta)
  r = r_W$r; W = r_W$W
  hat_lambda = r / W
  return(hat_lambda)
}

get_CI <- function(U, Delta){
  hat_lambda <- get_est(U,Delta)
  r_W <- get_r_W(U,T)
  r <- r_W$r
  ci95_lb <- hat_lambda * exp(-1.96 * sqrt(1/r))
  ci95_ub <- hat_lambda * exp(1.96 * sqrt(1/r))
  return(c(ci95_lb, ci95_ub))
}

