# setwd("~/Dropbox (Harvard University)/Xiang_Iav/code")
library(tidyverse)
library(survival)
source("cox-utils.R")

# Parameters
n <- 250
alpha <- 1.5
light_censoring = T
if (light_censoring == F){
  lambda_C <- 0.1
}
lambda_0 <- 1
tau <- 1
beta <- 1
beta_2 <- 2  # Coefficient for the non-linear term
R <- 200


########
### Non-time-varying
#########
# Linear
run_result(is_linear=T,
           is_time_varying=F)
# Time
#  secs for n = 100, R = 200: 85 secs
#  secs for n = 100, R = 100: 40 secs

# Non-linear
source("cox-utils.R")
run_result(is_linear=F,
           is_time_varying=F,
           causal = F, 
           light_censoring = T,
           outcome_type = "non-linear")
run_result(is_linear=F,
           is_time_varying=F,
           causal = T, 
           light_censoring = T,
           outcome_type = "non-linear")
# Time
#  secs for n = 100, R = 200: 115 secs
#  secs for n = 100, R = 100: 55 secs

# Non-linear, causal
run_result(is_linear=F,
           is_time_varying=F,
           causal=T)


### Time-varying
# linear, time_varying, non-causal, 
run_result(is_linear=T,
           is_time_varying=T,
           causal=F)

# non-linear, time_varying, non-causal, 
n = 1000; R = 800
run_result(is_linear=F,
           is_time_varying=T,
           causal=F)
# Time
# 91.7 secs for n = 100, R = 100
# 465 secs for n = 300, R = 100
# 401 secs n = 300, R = 100
# 1212 secs for n = 500, R = 200
# 6966 secs for n = 1000, R = 800

### non-linear, time_varying, causal, 
n = 200; R = 200
source("cox-utils.R")
run_result(is_linear=F,
           is_time_varying=T,
           causal=T)
# Time
# 101.5 secs for n = 100, R = 100
# 432.4 secs for n = 200, R = 200
# 1002.19 secs for n = 500, R = 200
# 6966 secs for n = 1000, R = 800




### Make DINA work
is_time_varying <- F; is_linear <- F
set.seed(123)
n <- 1000
simulated_data <- 
  generate_simulated_data(
    is_time_varying = is_time_varying, 
    is_linear = is_linear
  )
##  This is biased
m_timevar <- 
  coxph(formula = 
          Surv(U,Delta) ~ 
          W + X,
        data = simulated_data, 
        ties = "breslow")
tau_estimate <- 
  m_timevar_summary$coefficients[1,1]
tau_estimate # 0.807

##  This is unbiased
set.seed(123)
model <- 
  non_time_varying_estimate(
    simulated_data,
    causal=F,
    outcome_type = "non-linear")
  
tau_estimate_causal <- 
  summary(model)$coefficients[1,1]
tau_estimate_causal # 0.970

model_DINA_non_lin <- 
  non_time_varying_estimate(
    simulated_data,
    causal=T,
    outcome_type = "non-linear")
tau_estimate_causal <- 
  summary(model_DINA_non_lin)$coefficients[1,1]
tau_estimate_causal # 1.05

