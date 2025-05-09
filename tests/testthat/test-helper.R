


#' Calculate Minimum Relative Error Between True and Estimated Coefficients
#'
#' @description
#' Computes the minimum relative error between true and estimated coefficient vectors,
#' identifying the coefficient with the smallest relative difference.
#'
#' @param true_coef numeric vector of true coefficient values
#' @param est_coef numeric vector of estimated coefficient values
#'
#' @return A list containing:
#'   \item{min_relative_error}{minimum relative error value}
#'   \item{min_index}{index where minimum relative error occurs}
#'   \item{true_value}{true coefficient value at minimum error index}
#'   \item{estimated_value}{estimated coefficient value at minimum error index}
#'
#' @throws "True and estimated coefficients must have the same length"
#'
#' @examples
#' true_coef <- c(1, 2, 3, 4)
#' est_coef <- c(1.1, 2.2, 2.9, 4.1)
#' result <- calculate_min_relative_error(true_coef, est_coef)
#' print(result$min_relative_error)
#'
#' @export
calculate_min_relative_error <- function(true_coef, est_coef) {
  if (length(true_coef) != length(est_coef)) {
    stop("True and estimated coefficients must have the same length")
  }
  
  diff <- true_coef - as.vector(est_coef)
  relative_diff <- abs(diff / true_coef)
  min_idx <- which.min(relative_diff)
  
  list(
    min_relative_error = relative_diff[min_idx],
    min_index = min_idx,
    true_value = true_coef[min_idx],
    estimated_value = est_coef[min_idx]
  )
}


#' Calculate Maximum Relative Error Between True and Estimated Coefficients
#'
#' @description
#' Computes the maximum relative error between true and estimated coefficient vectors,
#' identifying the coefficient with the largest relative difference.
#'
#' @param true_coef numeric vector of true coefficient values
#' @param est_coef numeric vector of estimated coefficient values
#'
#' @return A list containing:
#'   \item{max_relative_error}{maximum relative error value}
#'   \item{max_index}{index where maximum relative error occurs}
#'   \item{true_value}{true coefficient value at maximum error index}
#'   \item{estimated_value}{estimated coefficient value at maximum error index}
#'
#' @throws "True and estimated coefficients must have the same length"
#'
#' @examples
#' true_coef <- c(1, 2, 3, 4)
#' est_coef <- c(1.1, 2.2, 2.9, 4.1)
#' result <- calculate_max_relative_error(true_coef, est_coef)
#' print(result$max_relative_error)
#'
#' @export
calculate_max_relative_error <- function(true_coef, est_coef) {
  if (length(true_coef) != length(est_coef)) {
    stop("True and estimated coefficients must have the same length")
  }
  
  diff <- true_coef - as.vector(est_coef)
  relative_diff <- abs(diff / true_coef)
  max_idx <- which.max(relative_diff)
  
  list(
    max_relative_error = relative_diff[max_idx],
    max_index = max_idx,
    true_value = true_coef[max_idx],
    estimated_value = est_coef[max_idx]
  )
}

#' Calculate Both Minimum and Maximum Relative Errors
#'
#' @description
#' Computes both minimum and maximum relative errors between true and estimated coefficient vectors.
#'
#' @param true_coef numeric vector of true coefficient values
#' @param est_coef numeric vector of estimated coefficient values
#'
#' @return A list containing:
#'   \item{min}{list with minimum relative error results}
#'   \item{max}{list with maximum relative error results}
#'
#' @examples
#' true_coef <- c(1, 2, 3, 4)
#' est_coef <- c(1.1, 2.2, 2.9, 4.1)
#' results <- calculate_relative_errors(true_coef, est_coef)
#' print(results$min$min_relative_error)
#' print(results$max$max_relative_error)
#'
#' @export
calculate_relative_errors <- function(true_coef, est_coef) {
  list(
    min = calculate_min_relative_error(true_coef, est_coef),
    max = calculate_max_relative_error(true_coef, est_coef)
  )
}