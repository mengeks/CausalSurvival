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
set.seed(123)
sim_data <- generate_simple_data(n = 500)

# Create time-varying dataset
data_tv <- create_time_varying_dataset(
  data = sim_data,
  event_time = "time_to_event",
  event = "event",
  tx_time = "time_to_tx",
  covariates = c("age", "comorbidity", "biomarker"),
  id_col = "id"
)

# Fit a TV-CSL model
model <- tvcsl(
  formula = Surv(tstart, tstop, status) ~ age + comorbidity + biomarker,
  data = data_tv,
  treatment_time = "time_to_tx",
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

### Sample Output

When running `summary(model)`, you'll see results similar to:

```
Time-Varying Causal Survival Learning (TV-CSL)
--------------------------------------------

Model Specification:
  Treatment effect form:  linear 
  Baseline hazard form:  linear 
  Propensity model:  cox-linear 
  Outcome model:  s-learner 
  Final model method:  coxph 
  Cross-validation folds:  5 

Covariates:
  age, comorbidity, biomarker 

Treatment Effect Coefficients:
  Intercept:  -0.5132 
  age:  0.3871 
  comorbidity:  0.1284 
  biomarker:  -0.289 
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

* This project is based on research supported by HBS research funding
* Thanks to D3Lab participants for their valuable input and feedback
