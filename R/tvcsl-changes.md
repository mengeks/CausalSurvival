# Time-Varying Causal Survival Learning (TV-CSL) Package Changes

## Overview of Changes

This document outlines the major changes made to the TV-CSL package to improve its:
- Modularity
- Readability 
- Performance
- Documentation
- Usability

## Major Structural Changes

### 1. Improved Function Organization

The package has been reorganized into logical sections:
- Data preprocessing
- Feature transformation
- Propensity score estimation
- Outcome model estimation
- Final model fitting
- Cross-validation and evaluation
- Main interface function

### 2. Standardized Variable and Function Naming

| Old Name | New Name | Description |
|----------|----------|-------------|
| `TV_CSL` | `tvcsl` | Main function, now follows R package naming conventions |
| `Cox model estimation` | `estimate_propensity_scores` | More descriptive of function purpose |
| `TV_CSL_nuisance` | `estimate_nuisance_parameters` | Clearer description of functionality |
| `fit_TV_CSL` | `fit_final_model` | More precise naming |
| `W` | `treatment_indicator` | Clearer variable name |
| `U` | `event_time` | More descriptive |
| `Delta` | `event_indicator` | More descriptive |
| `A` | `treatment_time` | More descriptive |
| `HTE_type` | `treatment_effect_form` | More intuitive |
| `eta_type` | `baseline_form` | More descriptive |
| `S_lasso`, `T_lasso` | `outcome_model` options | Consolidated into single parameter |

### 3. Improved Interface

#### Old Interface:
```r
TV_CSL(train_data, 
       test_data, 
       train_data_original, 
       HTE_type,
       eta_type,
       K, 
       prop_score_spec, 
       lasso_type, 
       regressor_spec, 
       final_model_method,
       HTE_spec,
       i = 0)
```

#### New Interface:
```r
tvcsl(formula = Surv(tstart, tstop, event_indicator) ~ age + gender + smoker,
      data = data_tv,
      treatment_time = "time_to_tx",
      treatment_effect_form = "constant",
      baseline_form = "linear",
      propensity_model = "cox-linear",
      outcome_model = "s-learner",
      final_model_method = "coxph",
      cv_folds = 5,
      id = "id",
      fast_lasso = TRUE)
```

Also added `quick_tvcsl()` for even simpler usage:

```r
quick_tvcsl(data = heart_data,
            event_time = "time_to_death",
            event_indicator = "death",
            treatment_time = "time_to_transplant",
            covariates = c("age", "gender", "smoker"))
```

### 4. Performance Improvements

- Added `fast_lasso` option to control computational complexity
- Optimized lambda sequence for CV Lasso
- Simplified cross-validation procedure
- Added option to control number of folds
- Improved memory usage by not storing intermediate results unless needed

### 5. Better Documentation

- Added comprehensive roxygen documentation for all functions
- Included examples
- Added parameter descriptions
- Created S3 methods for easier interaction with fitted models:
  - `print.tvcsl()`
  - `summary.tvcsl()`
  - `predict.tvcsl()`

### 6. Added Functions

- `create_time_varying_dataset()`: Properly exported function for creating time-varying datasets
- `evaluate_tvcsl()`: Function for evaluating model performance 
- `predict.tvcsl()`: Method for making predictions with new data

## Dependency Changes

The refactored package has the same core dependencies:
- survival
- glmnet
- dplyr
- splines
- purrr

## How to Update Existing Code

To update your existing code that uses the old interface:

1. Convert your data to time-varying format using `create_time_varying_dataset()`
2. Create a model formula with `Surv()` on the left side
3. Use `tvcsl()` instead of `TV_CSL()`
4. Use the new parameter names

Example conversion:

### Old Code:
```r
result <- TV_CSL(train_data = df_train, 
                test_data = df_test, 
                train_data_original = df_train_original,
                HTE_type = "constant",
                eta_type = "linear",
                K = 5, 
                prop_score_spec = "cox-linear-all-data",
                lasso_type = "S-lasso",
                regressor_spec = "linear", 
                final_model_method = "coxph",
                HTE_spec = "linear")
```

### New Code:
```r
model <- tvcsl(formula = Surv(tstart, tstop, event_indicator) ~ X.1 + X.2 + X.3,
              data = df_train,
              treatment_time = "A",
              treatment_effect_form = "constant",
              baseline_form = "linear",
              propensity_model = "cox-linear",
              outcome_model = "s-learner",
              final_model_method = "coxph",
              cv_folds = 5,
              id = "id",
              fast_lasso = TRUE)
```

Or even simpler:
```r
model <- quick_tvcsl(data = df_train_original,
                   event_time = "U", 
                   event_indicator = "Delta",
                   treatment_time = "A",
                   covariates = c("X.1", "X.2", "X.3"))
```
