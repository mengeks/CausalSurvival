# TV-CSL: Time-Varying Causal Survival Learning

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

TV-CSL is an R package for estimating causal effects in survival analysis settings with time-varying treatments. This package implements the methods described in the paper "Time-Varying Causal Survival Learning" by Meng & Bojinov.

## Overview

Time-Varying Causal Survival Learning (TV-CSL) uses double machine learning methods to estimate heterogeneous treatment effects in settings where treatment timing varies across units. This approach is particularly useful for:

- Heart transplant studies where patients have varying waiting times for organs
- Clinical trials with staggered treatment adoption
- Observational studies with time-varying interventions
- Business applications such as customer conversion after free trials

## Installation

```r
# Install required packages
install.packages(c("survival", "glmnet", "dplyr", "splines"))

# Clone the repository
# git clone https://github.com/yourusername/tvcsl.git

# Source the package
source("R/tvcsl-package.R")
```

## Quick Start Example

```r
library(survival)
library(dplyr)

# Source the package
source("R/tvcsl-package.R")

# Generate some simulated data
sim_data <- generate_simple_data(n = 200)

# Create time-varying dataset
data_tv <- create_time_varying_dataset(
  data = sim_data,
  event_time = "time_to_event",
  event_indicator = "event",
  treatment_time = "time_to_tx",
  covariates = c("age", "comorbidity", "biomarker"),
  id = "id"
)

# Fit a TV-CSL model
model <- quick_tvcsl(
  data = data_tv,
  event_time = "tstop",
  event_indicator = "event_indicator",
  treatment_time = "time_to_tx", 
  covariates = c("age", "comorbidity", "biomarker"),
  id = "id",
  treatment_effect_form = "linear"
)

# View results
summary(model)

# Make predictions for new patients
new_patients <- data.frame(
  age = c(45, 65),
  comorbidity = c(2, 3),
  biomarker = c(1.2, 0.8)
)
predict(model, new_patients)
```

## Data Simulation

The package includes a function to generate simulated data for testing:

```r
# Generate simple simulated data with various treatment effect patterns
simulate_data <- function(n = 500, 
                         treatment_effect = "linear", 
                         baseline_form = "non-linear",
                         seed = 123) {
  
  set.seed(seed)
  
  # Generate covariates
  data <- data.frame(
    id = 1:n,
    age = rnorm(n, mean = 50, sd = 10),
    comorbidity = rpois(n, lambda = 2),
    biomarker = rnorm(n, mean = 1, sd = 0.3)
  )
  
  # Scale covariates for numeric stability
  data$age_scaled <- scale(data$age)
  data$biomarker_scaled <- scale(data$biomarker)
  
  # Baseline hazard based on form
  if (baseline_form == "linear") {
    eta_0 <- with(data, 0.5 + 0.2 * age_scaled + 0.1 * comorbidity - 0.3 * biomarker_scaled)
  } else {
    # Non-linear baseline hazard
    eta_0 <- with(data, 0.5 + 0.3 * age_scaled^2 + 
                  0.2 * log(comorbidity + 1) - 0.3 * sin(biomarker_scaled * pi))
  }
  
  # Treatment effect based on type
  if (treatment_effect == "constant") {
    tau <- rep(-0.5, n)  # Constant treatment effect (negative = beneficial)
  } else if (treatment_effect == "linear") {
    tau <- with(data, -1 + 0.4 * age_scaled + 0.1 * comorbidity)  # Linear in covariates
  } else {
    # Non-linear treatment effect
    tau <- with(data, -0.8 + 0.5 * age_scaled^2 - 0.3 * (biomarker_scaled > 0))
  }
  
  # Generate treatment times
  # Higher comorbidity -> earlier treatment
  # Higher age -> later treatment (older patients wait longer)
  tx_hazard <- with(data, exp(0.5 - 0.1 * age_scaled + 0.3 * comorbidity))
  data$time_to_tx <- rexp(n, rate = tx_hazard)
  
  # Make some subjects untreated
  untreated <- sample(1:n, n/5)  # 20% untreated
  data$time_to_tx[untreated] <- Inf
  
  # Generate survival times under control
  control_hazard <- exp(eta_0)
  control_time <- rexp(n, rate = control_hazard)
  
  # Generate survival times under treatment (using treatment effect)
  treated_hazard <- exp(eta_0 + tau)
  treated_time <- rexp(n, rate = treated_hazard)
  
  # Combine to get observed times
  data$time_to_event <- ifelse(data$time_to_tx < Inf, 
                              pmin(data$time_to_tx, control_time) + 
                                ifelse(data$time_to_tx < control_time, 
                                      treated_time, 0),
                              control_time)
  
  # Add some censoring
  cens_time <- rexp(n, rate = 0.05)
  data$event <- as.integer(data$time_to_event <= cens_time)
  data$time_to_event <- pmin(data$time_to_event, cens_time)
  
  # Add true treatment effect for evaluation
  data$true_effect <- tau
  
  return(data)
}

# Generate data with linear treatment effect
data_linear <- simulate_data(n = 500, treatment_effect = "linear")

# View data
head(data_linear)
```

## Key Functions

### 1. Data Preparation

- `create_time_varying_dataset()`: Transforms standard survival data into time-varying format

### 2. Model Fitting

- `tvcsl()`: Main function for fitting TV-CSL models
- `quick_tvcsl()`: Simplified interface with sensible defaults

### 3. Evaluation and Prediction

- `predict.tvcsl()`: Make predictions with fitted models
- `evaluate_tvcsl()`: Calculate performance metrics for model evaluation

## Method Comparison with Real Data

When applied to heart transplant data, TV-CSL shows results consistent with traditional Cox models while offering methodological advantages:

| Effect | TV-CSL | Time-Varying Cox | Standard Cox |
|--------|--------|------------------|--------------|
| Main effect | -0.3332 | -0.5187 (p=0.071) | -0.5218 (p=0.073) |
| Age interaction | -0.3315 | -0.3833 (p<0.001) | -0.3256 (p<0.01) |
| Year interaction | 0.0525 | 0.0427 (p<0.001) | -0.0208 (p=0.107) |

All methods show that the benefit of transplantation decreases with age (negative age interaction), but the time-varying methods (TV-CSL and Time-Varying Cox) suggest improved transplant outcomes in recent years, while the standard Cox model does not capture this trend.

## Documentation

For complete documentation, see the function help pages by typing `?function_name` after loading the package.

## Advantages Over Traditional Methods

1. **Double Machine Learning**: Explicitly models waiting time for treatments, incorporating valuable information that improves accuracy

2. **Flexible Modeling**: Uses modern machine learning techniques (lasso) to better capture complex relationships in baseline hazards

3. **Proper Time-Varying Treatment**: Correctly accounts for the variable timing of treatment adoption

4. **Robustness to Confounding**: More robust to unmeasured confounding factors through orthogonalization techniques

## Citation

If you use this package in your research, please cite:

```
Meng, X., & Bojinov, I. (2025). Time-Varying Causal Survival Learning.
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Acknowledgments

* This project is based on research supported by [Funding Source]
* Thanks to [Collaborators] for their valuable input and feedback
