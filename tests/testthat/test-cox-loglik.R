

# Step 5: Run the Test
set.seed(123)
# Test 1: p = 1
n1 <- 1000  # Number of observations
p1 <- 1     # Number of covariates
true_beta1 <- 0.5  # True beta coefficient
init_beta1 <- 0    # Initial guess for beta

cat("\nRunning test for p = 1\n")
test_cox_loglik(
  n = n1, p = p1, 
  true_beta = true_beta1, 
  init_beta = init_beta1)

# Test 2: p = 3
n2 <- 1000  # Number of observations
p2 <- 3     # Number of covariates
true_beta2 <- c(0.5, -0.3, 0.2)  # True beta coefficients
init_beta2 <- rep(0, p2)         # Initial guess for beta

cat("\nRunning test for p = 3\n")
test_cox_loglik(n = n2, p = p2, true_beta = true_beta2, init_beta = init_beta2)




# Test 3: Offset
source(here::here("tests/test-helper-cox-loglik.R"))
# Test without offset
set.seed(123)
n <- 1000
p <- 3
true_beta <- c(0.5, -0.3, 0.2)
init_beta <- rep(0, p)

cat("\nRunning test for p = 3 with no offset\n")
test_cox_loglik(n = n, p = p, true_beta = true_beta, init_beta = init_beta)

# Test with offset
set.seed(123)
offset_vector <- rnorm(n, mean = 2, sd = 1)

cat("\nRunning test for p = 3 with offset\n")
test_cox_loglik(n = n, p = p, true_beta = true_beta, init_beta = init_beta, offset = offset_vector)

