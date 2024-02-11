#' WP method
#'
#' This function implements an R0 estimation due to White and Pagano (Statistics
#' in Medicine, 2008). The method is based on maximum likelihood estimation in a
#' Poisson transmission model. See details for important implementation notes.
#'
#' This method is based on a Poisson transmission model, and hence may be most
#' most valid at the beginning of an epidemic. In their model, the serial
#' distribution is assumed to be discrete with a finite number of posible
#' values. In this implementation, if \code{mu} is not {NA}, the serial
#' distribution is taken to be a discretized version of a gamma distribution
#' with mean \code{mu}, shape parameter one, and largest possible value based on
#' parameter \code{tol}. When \code{mu} is \code{NA}, the function implements a
#' grid search algorithm to find the maximum likelihood estimator over all
#' possible gamma distributions with unknown mean and variance, restricting
#' these to a prespecified grid (see \code{search} parameter).
#'
#' When the serial distribution is known (i.e., \code{mu} is not \code{NA}),
#' sensitivity testing of \code{mu} is strongly recommended. If the serial
#' distribution is unknown (i.e., \code{mu} is \code{NA}), the likelihood
#' function can be flat near the maximum, resulting in numerical instability of
#' the optimizer. When \code{mu} is \code{NA}, the implementation takes
#' considerably longer to run. Users should be careful about units of time
#' (e.g., are counts observed daily or weekly?) when implementing.
#'
#' The model developed in White and Pagano (2008) is discrete, and hence the
#' serial distribution is finite discrete. In our implementation, the input
#' value \code{mu} is that of a continuous distribution. The algorithm
#' discretizes this input when \code{mu} is not \code{NA}, and hence the mean of
#' the serial distribution returned in the list \code{SD} will differ from
#' \code{mu} somewhat. That is to say, if the user notices that the input
#' \code{mu} and output mean of \code{SD} are different, this is to be expected,
#' and is caused by the discretization.
#'
#' @param NT Vector of case counts.
#' @param mu Mean of the serial distribution (needs to match case counts in time
#'           units; for example, if case counts are weekly and the serial
#'           distribution has a mean of seven days, then \code{mu} should be set
#'           to one). The default value of \code{mu} is set to \code{NA}.
#' @param search List of default values for the grid search algorithm. The list
#'               includes three elements: the first is \code{B}, which is the
#'               length of the grid in one dimension; the second is
#'               \code{scale.max}, which is the largest possible value of the
#'               scale parameter; and the third is \code{shape.max}, which is
#'               the largest possible value of the shape parameter. Defaults to
#'               \code{B = 100, scale.max = 10, shape.max = 10}. For both shape
#'               and scale, the smallest possible value is 1/\code{B}.
#' @param tol Cutoff value for cumulative distribution function of the
#'            pre-discretization gamma serial distribution. Defaults to 0.999
#'            (i.e. in the discretization, the maximum is chosen such that the
#'            original gamma distribution has cumulative probability of no more
#'            than 0.999 at this maximum).
#'
#' @return \code{WP} returns a list containing the following components:
#'         \code{Rhat} is the estimate of R0, and \code{SD} is either the
#'         discretized serial distribution (if \code{mu} is not \code{NA}), or
#'         the estimated discretized serial distribution (if \code{mu} is
#'         \code{NA}). The list also returns the variable \code{check}, which is
#'         equal to the number of non-unique maximum likelihood estimators. The
#'         serial distribution \code{SD} is returned as a list made up of
#'         \code{supp} (the support of the distribution) and \code{pmf} (the
#'         probability mass function).
#'
#' @examples
#' # Weekly data.
#' NT <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#'
#' # Obtain R0 when the serial distribution has a mean of five days.
#' res1 <- WP(NT, mu = 5 / 7)
#' res1$Rhat
#'
#' # Obtain R0 when the serial distribution has a mean of three days.
#' res2 <- WP(NT, mu = 3 / 7)
#' res2$Rhat
#'
#' # Obtain R0 when the serial distribution is unknown.
#' # NOTE: This implementation will take longer to run.
#' res3 <- WP(NT)
#' res3$Rhat
#'
#' # Find the mean of the estimated serial distribution.
#' serial <- res3$SD
#' sum(serial$supp * serial$pmf)
#'
#' @importFrom stats pexp qexp
#'
#' @export
WP <- function(NT, mu = NA,
               search = list(B = 100, shape.max = 10, scale.max = 10),
               tol = 0.999) {
  if (is.na(mu)) {
    print("You have assumed that the serial distribution is unknown.")
    res <- WP_unknown(NT, B = search$B, shape.max = search$shape.max,
                      scale.max = search$scale.max, tol = tol)
    Rhat <- res$Rhat
    p <- res$p
    range.max <- res$range.max
    JJ <- res$JJ
  } else {
    print("You have assumed that the serial distribution is known.")
    range.max <- ceiling(qexp(tol, rate = 1 / mu))
    p <- diff(pexp(0:range.max, 1 / mu))
    p <- p / sum(p)
    res <- WP_known(NT = NT, p = p)
    Rhat <- res
    JJ <- NA
  }

  return(list(Rhat = Rhat,
              check = length(JJ),
              SD = list(supp = 1:range.max, pmf = p)))
}

#' WP method background function WP_known
#'
#' This is a background/internal function called by \code{WP}. It computes the
#' maximum likelihood estimator of R0 assuming that the serial distribution is
#' known and finite discrete.
#'
#' @param NT Vector of case counts.
#' @param p Discretized version of the serial distribution.
#'
#' @return The function returns the maximum likelihood estimator of R0.
#'
#' @noRd
WP_known <- function(NT, p) {
  k <- length(p)
  TT <- length(NT) - 1
  mu_t <- rep(0, TT)

  for (i in 1:TT) {
    Nt <- NT[i:max(1, i - k + 1)]
    mu_t[i] <- sum(p[1:min(k, i)] * Nt)
  }

  Rhat <- sum(NT[-1]) / sum(mu_t)
  return(Rhat)
}

#' WP method background function WP_unknown
#'
#' This is a background/internal function called by \code{WP}. It computes the
#' maximum likelihood estimator of R0 assuming that the serial distribution is
#' unknown but comes from a discretized gamma distribution. The function then
#' implements a simple grid search algorithm to obtain the maximum likelihood
#' estimator of R0 as well as the gamma parameters.
#'
#' @param NT Vector of case counts.
#' @param B Length of grid for shape and scale (grid search parameter).
#' @param shape.max Maximum shape value (grid \code{search} parameter).
#' @param scale.max Maximum scale value (grid \code{search} parameter).
#' @param tol cutoff value for cumulative distribution function of the serial
#'            distribution (defaults to 0.999).
#'
#' @return The function returns \code{Rhat}, the maximum likelihood estimator of
#'         R0, as well as the maximum likelihood estimator of the discretized
#'         serial distribution given by \code{p} (the probability mass function)
#'         and \code{range.max} (the distribution has support on the integers
#'         one to \code{range.max}). The function also returns \code{resLL} (all
#'         values of the log-likelihood) at \code{shape} (grid for shape
#'         parameter) and at \code{scale} (grid for scale parameter), as well as
#'         \code{resR0} (the full vector of maximum likelihood estimators),
#'         \code{JJ} (the locations for the likelihood for these), and \code{J0}
#'         (the location for the maximum likelihood estimator \code{Rhat}). If
#'         \code{JJ} and \code{J0} are not the same, this means that the maximum
#'         likelihood estimator is not unique.
#'
#' @importFrom stats pgamma qgamma
#'
#' @noRd
WP_unknown <- function(NT, B = 100, shape.max = 10, scale.max = 10,
                       tol = 0.999) {
  shape <- seq(0, shape.max, length.out = B + 1)
  scale <- seq(0, scale.max, length.out = B + 1)
  shape <- shape[-1]
  scale <- scale[-1]

  resLL <- matrix(0, B, B)
  resR0 <- matrix(0, B, B)

  for (i in 1:B)
    for (j in 1:B) {
      range.max <- ceiling(qgamma(tol, shape = shape[i], scale = scale[j]))
      p <- diff(pgamma(0:range.max, shape = shape[i], scale = scale[j]))
      p <- p / sum(p)
      mle <- WP_known(NT, p)
      resLL[i, j] <- computeLL(p, NT, mle)
      resR0[i, j] <- mle
    }

  J0 <- which.max(resLL)
  R0hat <- resR0[J0]
  JJ <- which(resLL == resLL[J0], arr.ind = TRUE)
  range.max <- ceiling(qgamma(tol, shape = shape[JJ[1]], scale = scale[JJ[2]]))
  p <- diff(pgamma(0:range.max, shape = shape[JJ[1]], scale = scale[JJ[2]]))
  p <- p / sum(p)

  return(list(Rhat = R0hat, J0 = J0, ll = resLL, Rs = resR0, scale = scale,
              shape = shape, JJ = JJ, p = p, range.max = range.max))
}

#' WP method background function computeLL
#'
#' This is a background/internal function called by \code{WP}. It computes the
#' log-likelihood.
#'
#' @param p Discretized version of the serial distribution.
#' @param NT Vector of case counts.
#' @param R0 Basic reproductive ratio.
#'
#' @return This function returns the log-likelihood at the input variables and
#'         parameters.
#'
#' @noRd
computeLL <- function(p, NT, R0) {
  k <- length(p)
  TT <- length(NT) - 1
  mu_t <- rep(0, TT)

  for (i in 1:TT) {
    Nt <- NT[i:max(1, i - k + 1)]
    mu_t[i] <- sum(p[1:min(k, i)] * Nt)
  }

  mu_t <- R0 * mu_t
  LL <- sum(NT[-1] * log(mu_t)) - sum(mu_t)

  return(LL)
}
