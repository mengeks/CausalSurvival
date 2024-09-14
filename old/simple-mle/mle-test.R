library(testthat)
setwd("~/Dropbox (Harvard University)/Xiang_Iav/Lu-Tian-Course")
source("mle-utils.R")

## Test usage of r.pw_exp
# tmp <- VirtualPop::r.pw_exp(n=1000,
#                             breakpoints=c(0,10, 100),
#                             rates=c(1/10, 1/5))
# hist(tmp)


## Test datagen
test_that("Mean of the generated data should be between the min rate and the max rate",
          {
            one_sample <-
              gen_one_sample(n = 1000,
                             lambda = c(1/2, 1/1),
                             lambda_c = 1/3,
                             lambda_a = 1/1)
            expect_gte(mean(one_sample$T), 1)
            expect_gte(2, mean(one_sample$T))
          })

beta <- 0.3
alpha <- 0.3
X <- runif(1000)
source("mle-utils.R")
one_sample <-
  gen_one_sample(n = 1000,
                 lambda = data.frame(lambda_0 = 1/2 * exp(beta*X),
                                     lambda_1 = 1/1 * exp(beta*X)),
                 lambda_c = 1/3,
                 lambda_a = 1/1 * exp(alpha*X))
expect_gte(mean(one_sample$T), 1)
expect_gte(2, mean(one_sample$T))


C_test <- c(50,50,100,100, 100)
T_test <- rep(75,5)
# A_test <-


U_test <- pmin(T_test, C_test); Delta_test <- T_test <= C_test
r_W_test <-
  get_r_W(U = U_test,
          Delta = Delta_test)
stopifnot(r_W_test$r == 3,
          r_W_test$W == 325)
est_test <-
  get_est(
    U = U_test,
    Delta = Delta_test
  )
stopifnot(est_test == 3/325)


### Illustration using unit2.pdf lecture note
# one sample
# Data:
r = 50 # 95 people, 50 uncensored people
W = 80.8 # 80.8 years, sum of censored survival
# time for 95 people
# i.e., survival times for the 50 people
#       + censored time for 45 people
#   = 80.8 years
hat_lambda = r / W # parametric hazard = 0.618
ci95_lb <- hat_lambda * exp(-1.96 * sqrt(1/r))
ci95_ub <- hat_lambda * exp(1.96 * sqrt(1/r))

