#' Incidence Decay and Exponential Adjustment (IDEA)
#'
#' This function implements a least squares estimation method of R0 due to
#' Fisman et al. (PloS One, 2013). See details for implementation notes.
#'
#' This method is closely related to that implemented in [id()]. The method is
#' based on an incidence decay model. The estimate of R0 is the value which
#' minimizes the sum of squares between observed case counts and case counts
#' expected under the model.
#'
#' This method is based on an approximation of the SIR model, which is most
#' valid at the beginning of an epidemic. The method assumes that the mean of
#' the serial distribution (sometimes called the serial interval) is known. The
#' final estimate can be quite sensitive to this value, so sensitivity testing
#' is strongly recommended. Users should be careful about units of time (e.g.,
#' are counts observed daily or weekly?) when implementing.
#'
#' @param cases Vector of case counts. The vector must be of length at least two
#'   and only contain positive integers.
#' @param mu Mean of the serial distribution. This must be a positive number.
#'   The value should match the case counts in time units. For example, if case
#'   counts are weekly and the serial distribution has a mean of seven days,
#'   then `mu` should be set to `1`. If case counts are daily and the serial
#'   distribution has a mean of seven days, then `mu` should be set to `7`.
#'
#' @return An estimate of the basic reproduction number (R0).
#'
#' @references [Fisman et al. (PloS One, 2013)](
#' https://doi.org/10.1371/journal.pone.0083622)
#'
#' @seealso [id()] for a similar method.
#'
#' @export
#'
#' @examples
#' # Weekly data.
#' cases <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#'
#' # Obtain R0 when the serial distribution has a mean of five days.
#' idea(cases, mu = 5 / 7)
#'
#' # Obtain R0 when the serial distribution has a mean of three days.
#' idea(cases, mu = 3 / 7)
idea <- function(cases, mu) {
  s <- seq_along(cases) / mu

  x1 <- sum(s)
  x2 <- sum(s^2)
  x3 <- log(cases)

  y1 <- x2 * sum(x3 / s) - x1 * sum(x3)
  y2 <- x2 * length(cases) - x1^2

  exp(y1 / y2)
}
