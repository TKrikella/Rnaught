#' White and Pagano (WP)
#'
#' This function implements an R0 estimation due to White and Pagano (Statistics
#' in Medicine, 2008). The method is based on maximum likelihood estimation in a
#' Poisson transmission model. See details for important implementation notes.
#'
#' This method is based on a Poisson transmission model, and hence may be most
#' most valid at the beginning of an epidemic. In their model, the serial
#' distribution is assumed to be discrete with a finite number of possible
#' values. In this implementation, if `mu` is not `NA`, the serial distribution
#' is taken to be a discretized version of a gamma distribution with shape
#' parameter `1` and scale parameter `mu` (and hence mean `mu`). When `mu` is
#' `NA`, the function implements a grid search algorithm to find the maximum
#' likelihood estimator over all possible gamma distributions with unknown shape
#' and scale, restricting these to a prespecified grid (see the parameters
#' `grid_length`, `max_shape` and `max_scale`). In both cases, the largest value
#' of the support is chosen such that the cumulative distribution function of
#' the original (pre-discretized) gamma distribution has cumulative probability
#' of no more than 0.999 at this value.
#'
#' When the serial distribution is known (i.e., `mu` is not `NA`), sensitivity
#' testing of `mu` is strongly recommended. If the serial distribution is
#' unknown (i.e., `mu` is `NA`), the likelihood function can be flat near the
#' maximum, resulting in numerical instability of the optimizer. When `mu` is
#' `NA`, the implementation takes considerably longer to run. Users should be
#' careful about units of time (e.g., are counts observed daily or weekly?) when
#' implementing.
#'
#' The model developed in White and Pagano (2008) is discrete, and hence the
#' serial distribution is finite discrete. In our implementation, the input
#' value `mu` is that of a continuous distribution. The algorithm discretizes
#' this input, and so the mean of the estimated serial distribution returned
#' (when `serial` is set to `TRUE`) will differ from `mu` somewhat. That is to
#' say, if the user notices that the input `mu` and the mean of the estimated
#' serial distribution are different, this is to be expected, and is caused by
#' the discretization.
#'
#' @param cases Vector of case counts. The vector must be of length at least two
#'   and only contain positive integers.
#' @param mu Mean of the serial distribution (defaults to `NA`). This must be a
#'   positive number or `NA`. If a number is specified, the value should match
#'   the case counts in time units. For example, if case counts are weekly and
#'   the serial distribution has a mean of seven days, then `mu` should be set
#'   to `1`. If case counts are daily and the serial distribution has a mean of
#'   seven days, then `mu` should be set to `7`.
#' @param serial Whether to return the estimated serial distribution in addition
#'   to the estimate of R0 (defaults to `FALSE`). This must be a value identical
#'   to `TRUE` or `FALSE`.
#' @param grid_length The length of the grid in the grid search (defaults to
#'   100). This must be a positive integer. It will only be used if `mu` is set
#'   to `NA`. The grid search will go through all combinations of the shape and
#'   scale parameters for the gamma distribution, which are `grid_length` evenly
#'   spaced values from `0` (exclusive) to `max_shape` and `max_scale`
#'   (inclusive), respectively. Note that larger values will result in a longer
#'   search time.
#' @param max_shape The largest possible value of the shape parameter in the
#'   grid search (defaults to 10). This must be a positive number. It will only
#'   be used if `mu` is set to `NA`. Note that larger values will result in a
#'   longer search time, and may cause numerical instabilities.
#' @param max_scale The largest possible value of the scale parameter in the
#'   grid search (defaults to 10). This must be a positive number. It will only
#'   be used if `mu` is set to `NA`. Note that larger values will result in a
#'   longer search time, and may cause numerical instabilities.
#'
#' @return If `serial` is identical to `TRUE`, a list containing the following
#'   components is returned:
#'   * `r0` - the estimate of R0
#'   * `supp` - the support of the estimated serial distribution
#'   * `pmf` - the probability mass function of the estimated serial
#'     distribution
#'
#'   Otherwise, if `serial` is identical to `FALSE`, only the estimate of R0 is
#'   returned.
#'
#' @references [White and Pagano (Statistics in Medicine, 2008)](
#' https://doi.org/10.1002/sim.3136)
#'
#' @seealso `vignette("wp_serial", package="Rnaught")` for examples of using the
#'   serial distribution.
#'
#' @importFrom stats pgamma qgamma
#'
#' @export
#'
#' @examples
#' # Weekly data.
#' cases <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#'
#' # Obtain R0 when the serial distribution has a mean of five days.
#' wp(cases, mu = 5 / 7)
#'
#' # Obtain R0 when the serial distribution has a mean of three days.
#' wp(cases, mu = 3 / 7)
#'
#' # Obtain R0 when the serial distribution is unknown.
#' # Note that this will take longer to run than when `mu` is known.
#' wp(cases)
#'
#' # Same as above, but specify custom grid search parameters. The larger any of
#' # the parameters, the longer the search will take, but with potentially more
#' # accurate estimates.
#' wp(cases, grid_length = 40, max_shape = 4, max_scale = 4)
#'
#' # Return the estimated serial distribution in addition to the estimate of R0.
#' estimate <- wp(cases, serial = TRUE)
#'
#' # Display the estimate of R0, as well as the support and probability mass
#' # function of the estimated serial distribution returned by the grid search.
#' estimate$r0
#' estimate$supp
#' estimate$pmf
wp <- function(cases, mu = NA, serial = FALSE,
               grid_length = 100, max_shape = 10, max_scale = 10) {
  if (is.na(mu)) {
    search <- wp_search(cases, grid_length, max_shape, max_scale)
    r0 <- search$r0
    serial_supp <- search$supp
    serial_pmf <- search$pmf
  } else {
    max_range <- ceiling(qgamma(0.999, shape = 1, scale = mu))
    serial_supp <- seq_len(max_range)
    serial_pmf <- diff(pgamma(0:max_range, shape = 1, scale = mu))
    serial_pmf <- serial_pmf / sum(serial_pmf)
    r0 <- sum(cases[-1]) / sum(wp_mu_t_sigma(cases, serial_pmf))
  }

  if (!serial) {
    return(r0)
  }
  list(r0 = r0, supp = serial_supp, pmf = serial_pmf)
}

#' White and Pagano (WP) Grid Search
#'
#' This is a background/internal function called by [wp()]. It computes the
#' maximum likelihood estimator of R0 assuming that the serial distribution is
#' unknown (i.e., [wp()] is called with `mu` set to `NA`) but comes from a
#' discretized gamma distribution. The function implements a simple grid search
#' to obtain the maximum likelihood estimator of R0 as well as the gamma
#' parameters.
#'
#' @param cases Vector of case counts.
#' @param grid_length The length of the grid in the grid search.
#' @param max_shape The largest possible value of the shape parameter in the
#'   grid search.
#' @param max_scale The largest possible value of the scale parameter in the
#'   grid search.
#'
#' @return A list containing the following components is returned:
#'   * `r0` - the estimate of R0
#'   * `supp` - the support of the estimated serial distribution
#'   * `pmf` - the probability mass function of the estimated serial
#'     distribution
#'
#' @references
#' [White and Pagano (Statistics in Medicine, 2008)](
#' https://doi.org/10.1002/sim.3136)
#'
#' @seealso [wp()] for the function in which this grid search is called.
#'
#' @importFrom stats pgamma qgamma
#'
#' @noRd
wp_search <- function(cases, grid_length, max_shape, max_scale) {
  shapes <- seq(0, max_shape, length.out = grid_length + 1)[-1]
  scales <- seq(0, max_scale, length.out = grid_length + 1)[-1]

  best_log_like <- -Inf
  best_serial_pmf <- NA
  best_max_range <- NA
  r0 <- NA

  for (i in seq_len(grid_length)) {
    for (j in seq_len(grid_length)) {
      max_range <- ceiling(qgamma(0.999, shape = shapes[i], scale = scales[j]))

      serial_pmf <- diff(
        pgamma(0:max_range, shape = shapes[i], scale = scales[j])
      )
      serial_pmf <- serial_pmf / sum(serial_pmf)

      mu_t_sigma <- wp_mu_t_sigma(cases, serial_pmf)
      mle <- sum(cases[-1]) / sum(mu_t_sigma)
      mu_t <- mle * mu_t_sigma

      log_like <- sum(cases[-1] * log(mu_t)) - sum(mu_t)
      if (log_like > best_log_like) {
        best_log_like <- log_like
        best_serial_pmf <- serial_pmf
        best_max_range <- max_range
        r0 <- mle
      }
    }
  }

  list(r0 = r0, supp = seq_len(best_max_range), pmf = best_serial_pmf)
}

#' White and Pagano (WP) Mu Function Helper
#'
#' This is a background/internal function called by [wp()] and [wp_search()]. It
#' computes the sum inside the function `mu(t)`, which is present in the log
#' likelihood function. See the referenced article for more details.
#'
#' @param cases Vector of case counts.
#' @param serial_pmf The probability mass function of the serial distribution.
#'
#' @return The sum inside the function `mu(t)` of the log likelihood.
#'
#' @references
#' [White and Pagano (Statistics in Medicine, 2008)](
#' https://doi.org/10.1002/sim.3136)
#'
#' @seealso [wp()] and [wp_search()] for the functions which require this sum.
#'
#' @noRd
wp_mu_t_sigma <- function(cases, serial_pmf) {
  mu_t_sigma <- rep(0, length(cases) - 1)
  for (i in seq_len(length(cases) - 1)) {
    mu_t_sigma[i] <- sum(
      serial_pmf[seq_len(min(length(serial_pmf), i))] *
        cases[i:max(1, i - length(serial_pmf) + 1)]
    )
  }
  mu_t_sigma
}
