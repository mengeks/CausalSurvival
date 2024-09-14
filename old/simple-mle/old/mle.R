setwd("~/Dropbox (Harvard University)/Xiang_Iav/code/simple-mle")
source("mle-utils.R")

# A simulation to show the right coverage
one_sample <-
  gen_one_sample(n = 1000,
                 lambda_0 = 0.5)
U <- one_sample$U; Delta <- one_sample$Delta
get_est(
  U = U,
  Delta = Delta
) # should be around 0.5

get_CI(
  U = U,
  Delta = Delta
)

## Test coverage
R <- 1000
ests <- coverages <- numeric(R)
n = 1000; lambda_0 = 0.5
lambda_c <- 1/3
for (r in 1:R){
  set.seed(r * 100 + 23)
  one_sample <-
    gen_one_sample(n = n,
                   lambda_0 = lambda_0,
                   lambda_c=lambda_c)
  U <- one_sample$U; Delta <- one_sample$Delta
  ests[r] = get_est(
    U = U,
    Delta = Delta
  )
  ci <- get_CI(
    U = U,
    Delta = Delta
  )
  coverages[r] <-
    (ci[1] <= lambda_0) &
    (lambda_0 <= ci[2])
}

mean(ests)
mean(coverages)

