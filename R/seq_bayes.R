#' Sequential Bayes (seqB)
#'
#' This function implements a sequential Bayesian estimation method of R0 due to
#' Bettencourt and Riberio (PloS One, 2008). See details for important
#' implementation notes.
#'
#' The method sets a uniform prior distribution on R0 with possible values
#' between `0` and `kappa`, discretized to a fine grid. The distribution of R0
#' is then updated sequentially, with one update for each new case count
#' observation. The final estimate of R0 is the mean of the (last) posterior
#' distribution. The prior distribution is the initial belief of the
#' distribution of R0, which is the uninformative uniform distribution with
#' values between `0` and `kappa`. Users can change the value of `kappa` only
#' (i.e., the prior distribution cannot be changed from the uniform). As more
#' case counts are observed, the influence of the prior distribution should
#' lessen on the final estimate.
#'
#' This method is based on an approximation of the SIR model, which is most
#' valid at the beginning of an epidemic. The method assumes that the mean of
#' the serial distribution (sometimes called the serial interval) is known. The
#' final estimate can be quite sensitive to this value, so sensitivity testing
#' is strongly recommended. Users should be careful about units of time (e.g.,
#' are counts observed daily or weekly?) when implementing.
#'
#' Our code has been modified to provide an estimate even if case counts equal
#' to zero are present in some time intervals. This is done by grouping the
#' counts over such periods of time. Without grouping, and in the presence of
#' zero counts, no estimate can be provided.
#'
#' @param cases Vector of case counts. The vector must only contain non-negative
#'   integers, and have at least two positive integers.
#' @param mu Mean of the serial distribution. This must be a positive number.
#'   The value should match the case counts in time units. For example, if case
#'   counts are weekly and the serial distribution has a mean of seven days,
#'   then `mu` should be set to `1`. If case counts are daily and the serial
#'   distribution has a mean of seven days, then `mu` should be set to `7`.
#' @param kappa Largest possible value of the uniform prior (defaults to `20`).
#'   This must be a number greater than or equal to `1`. It describes the prior
#'   belief on the ranges of R0, and should be set to a higher value if R0 is
#'   believed to be larger.
#' @param post Whether to return the posterior distribution of R0 instead of the
#'   estimate of R0 (defaults to `FALSE`). This must be a value identical to
#'   `TRUE` or `FALSE`.
#'
#' @return If `post` is identical to `TRUE`, a list containing the following
#'   components is returned:
#'   * `supp` - the support of the posterior distribution of R0
#'   * `pmf` - the probability mass function of the posterior distribution
#'
#'   Otherwise, if `post` is identical to `FALSE`, only the estimate of R0 is
#'   returned. Note that the estimate is equal to `sum(supp * pmf)` (i.e., the
#'   posterior mean).
#'
#' @references
#' [Bettencourt and Riberio (PloS One, 2008)](
#' https://doi.org/10.1371/journal.pone.0002185)
#'
#' @export
#'
#' @examples
#' # Weekly data.
#' cases <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#'
#' # Obtain R0 when the serial distribution has a mean of five days.
#' seq_bayes(cases, mu = 5 / 7)
#'
#' # Obtain R0 when the serial distribution has a mean of three days.
#' seq_bayes(cases, mu = 3 / 7)
#'
#' # Obtain R0 when the serial distribution has a mean of seven days, and R0 is
#' # believed to be at most 4.
#' estimate <- seq_bayes(cases, mu = 1, kappa = 4)
#'
#' # Same as above, but return the posterior distribution instead of the
#' # estimate.
#' posterior <- seq_bayes(cases, mu = 1, kappa = 4, post = TRUE)
#'
#' # Note that the following always holds:
#' estimate == sum(posterior$supp * posterior$pmf)
seq_bayes <- function(cases, mu, kappa = 20, post = FALSE) {
  if (any(cases == 0)) {
    times <- which(cases > 0)
    if (length(times) < 2) {
      stop("Vector of case counts must contain at least two positive integers.")
    }
    cases <- cases[times]
  } else {
    times <- seq_along(cases)
  }

  support <- seq(0, kappa, 0.01)
  tau <- diff(times)

  prior <- rep(1, kappa / 0.01 + 1)
  prior <- prior / sum(prior)
  posterior <- seq(0, length(prior))

  for (i in seq_len(length(cases) - 1)) {
    lambda <- tau[i] / mu * (support - 1) + log(cases[i])
    log_like <- cases[i + 1] * lambda - exp(lambda)
    max_log_like <- max(log_like)

    if (max_log_like > 700) {
      log_like <- log_like - max_log_like + 700
    }

    posterior <- exp(log_like) * prior
    posterior <- posterior / sum(posterior)
    prior <- posterior
  }

  if (!post) {
    return(sum(support * posterior))
  }
  list(supp = support, pmf = posterior)
}
