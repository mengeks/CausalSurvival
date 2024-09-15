library(rlearner)
library(MASS)
library(here)
source("R/R-learner-helper.R")

n_list <- c(250, 500, 1000)
R_list <- c(200, 500)
outcomes <- c("linear-interaction", "log")

for (n in n_list) {
  for (R in R_list) {
    for (outcome in outcomes) {
      run_simulation(n, R, outcome)
    }
  }
}
