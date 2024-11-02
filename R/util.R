#' Case Counts Validation
#'
#' This is an internal function called by the estimators. It validates the
#' supplied case counts by ensuring it is a vector of integers of length at
#' least `min_length` with entries greater than or equal to `min_count`.
#' Execution is halted if these requirements are not satisfied.
#'
#' @param cases The case counts to be validated.
#' @param min_length The minimum length of the vector of case counts.
#' @param min_count The minimum value of the case count vector's entries.
#'
#' @noRd
validate_cases <- function(cases, min_length, min_count) {
  if (!is.vector(cases) || !is.numeric(cases) || any(floor(cases) != cases)) {
    stop("Case counts must be a vector of integers.", call. = FALSE)
  }
  if (length(cases) < min_length) {
    stop(paste("Case counts must have at least", min_length, "entries."),
      call. = FALSE
    )
  }
  if (any(cases < min_count)) {
    stop(paste0("Case counts cannot be less than ", min_count, "."),
      call. = FALSE
    )
  }
}

#' Real Number Testing
#'
#' This is an internal function which checks whether the given argument is a
#' real number.
#'
#' @param x The argument to test for being a real number.
#'
#' @return `TRUE` if the argument is a real number, `FALSE` otherwise.
#'
#' @noRd
is_real <- function(x) {
  is.vector(x) && is.numeric(x) && length(x) == 1
}

#' Integer Testing
#'
#' This is an internal function which checks whether the given argument is an
#' integer.
#'
#' @param n The argument to test for being an integer.
#'
#' @return `TRUE` if the argument is an integer, `FALSE` otherwise.
#'
#' @noRd
is_integer <- function(n) {
  is_real(n) && floor(n) == n
}
