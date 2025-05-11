#' @title Time-Varying Causal Survival Learning (TV-CSL)
#' @description A package for estimating causal effects in survival analysis settings with time-varying treatments.
#'
#' @docType package
#' @name tvcsl-package
#'
#' @importFrom survival coxph Surv
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom dplyr mutate filter select group_by ungroup arrange if_else left_join %>%
#' @importFrom stats as.formula coef predict model.matrix
#' @importFrom splines ns
#' @importFrom purrr map_dfr
#' @importFrom utils head tail
#' @importFrom stats na.omit

# Load required packages when sourcing directly
required_pkgs <- c("survival", "glmnet", "dplyr", "splines")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is required but not installed. ",
                "Please install it with: install.packages('", pkg, "')"))
  }
  library(pkg, character.only = TRUE)
}

# Load all package components
source("R/tvcsl-data-preprocessing.R")
source("R/tvcsl-feature-transform.R")
source("R/tvcsl-propensity-models.R")
source("R/tvcsl-outcome-models.R")
source("R/tvcsl-final-model.R")
source("R/tvcsl-cv-evaluation.R")
source("R/tvcsl-main.R")

# Custom implementations of S3method and export for use when sourcing directly
S3method <- function(generic, class) {
  # Register the S3 method by assigning the method function to the appropriate name
  method_name <- paste(generic, class, sep = ".")
  # No action needed when sourcing directly, as long as the methods are defined
  invisible(NULL)
}

export <- function(...) {
  # No action needed when sourcing directly
  invisible(NULL)
}

# Register S3 methods
S3method("print", "tvcsl")
S3method("summary", "tvcsl")
S3method("predict", "tvcsl")

# Export main functions
export("tvcsl")
export("quick_tvcsl")
export("create_time_varying_dataset")
export("evaluate_tvcsl")

#' Time-Varying Causal Survival Learning (TV-CSL)
#'
#' @description A package for estimating causal effects of time-varying treatments in survival analysis.
#' 
#' TV-CSL uses double machine learning methods to estimate heterogeneous treatment effects
#' in settings where treatment timing varies across units. This package implements the methods
#' described in the paper "Time-Varying Causal Survival Learning" by Meng & Bojinov.
#'
#' Key features:
#' - Support for time-varying treatments in survival analysis
#' - Double machine learning for robust causal inference
#' - Heterogeneous treatment effect estimation
#' - Flexible modeling of baseline hazards and propensity scores
#' 
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{tvcsl}}}{The main function for fitting TV-CSL models}
#'   \item{\code{\link{quick_tvcsl}}}{A simplified interface for fitting models with sensible defaults}
#'   \item{\code{\link{create_time_varying_dataset}}}{Transform standard survival data to time-varying format}
#'   \item{\code{\link{evaluate_tvcsl}}}{Evaluate model performance using test data}
#' }
#'
#' @examples
#' \dontrun{
#' # Create time-varying dataset
#' data_tv <- create_time_varying_dataset(
#'   data = heart_data,
#'   event_time = "time_to_death",
#'   event_indicator = "death",
#'   treatment_time = "time_to_transplant",
#'   covariates = c("age", "gender", "smoker")
#' )
#'
#' # Fit TV-CSL model
#' model <- tvcsl(
#'   formula = Surv(tstart, tstop, event_indicator) ~ age + gender + smoker,
#'   data = data_tv,
#'   treatment_time = "time_to_tx",
#'   treatment_effect_form = "linear"
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Make predictions
#' predictions <- predict(model, newdata = test_data)
#' }
#'
#' @name tvcsl-package
NULL

#' Time-Varying Causal Survival Learning (TV-CSL)
#'
#' @description A package for estimating causal effects of time-varying treatments in survival analysis.
#' 
#' TV-CSL uses double machine learning methods to estimate heterogeneous treatment effects
#' in settings where treatment timing varies across units. This package implements the methods
#' described in the paper "Time-Varying Causal Survival Learning" by Meng & Bojinov.
#'
#' Key features:
#' - Support for time-varying treatments in survival analysis
#' - Double machine learning for robust causal inference
#' - Heterogeneous treatment effect estimation
#' - Flexible modeling of baseline hazards and propensity scores
#' 
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{tvcsl}}}{The main function for fitting TV-CSL models}
#'   \item{\code{\link{quick_tvcsl}}}{A simplified interface for fitting models with sensible defaults}
#'   \item{\code{\link{create_time_varying_dataset}}}{Transform standard survival data to time-varying format}
#'   \item{\code{\link{evaluate_tvcsl}}}{Evaluate model performance using test data}
#' }
#'
#' @examples
#' \dontrun{
#' # Create time-varying dataset
#' data_tv <- create_time_varying_dataset(
#'   data = heart_data,
#'   event_time = "time_to_death",
#'   event_indicator = "death",
#'   treatment_time = "time_to_transplant",
#'   covariates = c("age", "gender", "smoker")
#' )
#'
#' # Fit TV-CSL model
#' model <- tvcsl(
#'   formula = Surv(tstart, tstop, event_indicator) ~ age + gender + smoker,
#'   data = data_tv,
#'   treatment_time = "time_to_tx",
#'   treatment_effect_form = "linear"
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Make predictions
#' predictions <- predict(model, newdata = test_data)
#' }
#'
#' @name tvcsl-package
NULL

#' Time-Varying Causal Survival Learning (TV-CSL)
#'
#' @description A package for estimating causal effects of time-varying treatments in survival analysis.
#' 
#' TV-CSL uses double machine learning methods to estimate heterogeneous treatment effects
#' in settings where treatment timing varies across units. This package implements the methods
#' described in the paper "Time-Varying Causal Survival Learning" by Meng & Bojinov.
#'
#' Key features:
#' - Support for time-varying treatments in survival analysis
#' - Double machine learning for robust causal inference
#' - Heterogeneous treatment effect estimation
#' - Flexible modeling of baseline hazards and propensity scores
#' 
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{tvcsl}}}{The main function for fitting TV-CSL models}
#'   \item{\code{\link{quick_tvcsl}}}{A simplified interface for fitting models with sensible defaults}
#'   \item{\code{\link{create_time_varying_dataset}}}{Transform standard survival data to time-varying format}
#'   \item{\code{\link{evaluate_tvcsl}}}{Evaluate model performance using test data}
#' }
#'
#' @examples
#' \dontrun{
#' # Create time-varying dataset
#' data_tv <- create_time_varying_dataset(
#'   data = heart_data,
#'   event_time = "time_to_death",
#'   event_indicator = "death",
#'   treatment_time = "time_to_transplant",
#'   covariates = c("age", "gender", "smoker")
#' )
#'
#' # Fit TV-CSL model
#' model <- tvcsl(
#'   formula = Surv(tstart, tstop, event_indicator) ~ age + gender + smoker,
#'   data = data_tv,
#'   treatment_time = "time_to_tx",
#'   treatment_effect_form = "linear"
#' )
#'
#' # View model summary
#' summary(model)
#'
#' # Make predictions
#' predictions <- predict(model, newdata = test_data)
#' }
#'
#' @name tvcsl-package
NULL