#' WP method background function WP_known
#'
#' This is a background/internal function called by \code{WP}. It computes the maximum
#' likelihood estimator of R0 assuming that the serial distribution is known and finite discrete.
#'
#' @param NT Vector of case counts.
#' @param p Discretized version of the serial distribution.
#'
#' @return The function returns the maximum likelihood estimator of R0.
#'
#' @keywords internal
WP_known <- function(NT, p) {
    k <- length(p)
    TT <- length(NT) - 1
	mu_t <- rep(0, TT)

    for (i in 1:TT) {
        Nt <- NT[i:max(1, i-k+1)]
        mu_t[i]	<- sum(p[1:min(k, i)] * Nt)
    }

    Rhat <- sum(NT[-1]) / sum(mu_t)
	return(Rhat)
}

#' WP method background function WP_unknown
#'
#' This is a background/internal function called by \code{WP}. It computes the maximum likelihood estimator
#' of R0 assuming that the serial distribution is unknown but comes from a discretized gamma distribution.
#' The function then implements a simple grid search algorithm to obtain the maximum likelihood estimator
#' of R0 as well as the gamma parameters.
#'
#' @param NT Vector of case counts.
#' @param B Length of grid for shape and scale (grid search parameter).
#' @param shape.max Maximum shape value (grid \code{search} parameter).
#' @param scale.max Maximum scale value (grid \code{search} parameter).
#' @param tol cutoff value for cumulative distribution function of the serial distribution (defaults to 0.999).
#'
#' @return The function returns \code{Rhat}, the maximum likelihood estimator of R0, as well as the maximum
#'         likelihood estimator of the discretized serial distribution given by \code{p} (the probability mass
#'         function) and \code{range.max} (the distribution has support on the integers one to \code{range.max}).
#'         The function also returns \code{resLL} (all values of the log-likelihood) at \code{shape} (grid for
#'         shape parameter) and at \code{scale} (grid for scale parameter), as well as \code{resR0} (the full
#'         vector of maximum likelihood estimators), \code{JJ} (the locations for the likelihood for these), and
#'         \code{J0} (the location for the maximum likelihood estimator \code{Rhat}). If \code{JJ} and \code{J0}
#'         are not the same, this means that the maximum likelihood estimator is not unique.
#'
#' @importFrom stats pgamma qgamma
#'
#' @keywords internal
WP_unknown <- function(NT, B=100, shape.max=10, scale.max=10, tol=0.999) {
	shape <- seq(0, shape.max, length.out=B+1)
	scale <- seq(0, scale.max, length.out=B+1)
	shape <- shape[-1]
	scale <- scale[-1]

	resLL <- matrix(0,B,B)
	resR0 <- matrix(0,B,B)

    for (i in 1:B) {
        for (j in 1:B) {
            range.max <- ceiling(qgamma(tol, shape=shape[i], scale=scale[j]))
            p <- diff(pgamma(0:range.max, shape=shape[i], scale=scale[j]))
            p <- p / sum(p)
            mle <- WP_known(NT, p)
            resLL[i,j] <- computeLL(p, NT, mle)
            resR0[i,j] <- mle
        }
    }
	
    J0 <- which.max(resLL)
    R0hat <- resR0[J0]
    JJ <- which(resLL == resLL[J0], arr.ind=TRUE)
    range.max <- ceiling(qgamma(tol, shape=shape[JJ[1]], scale=scale[JJ[2]]))
    p <- diff(pgamma(0:range.max, shape=shape[JJ[1]], scale=scale[JJ[2]]))
    p <- p / sum(p)
	
    return(list(Rhat=R0hat, J0=J0, ll=resLL, Rs=resR0, scale=scale, shape=shape, JJ=JJ, p=p, range.max=range.max))
}

#' WP method background function computeLL
#'
#' This is a background/internal function called by \code{WP}.  It computes the log-likelihood.
#'
#' @param NT Vector of case counts.
#' @param p Discretized version of the serial distribution.
#' @param R0 Basic reproductive ratio.
#'
#' @return This function returns the log-likelihood at the input variables and parameters.
#'
#' @keywords internal
computeLL <- function(p, NT, R0) {
    k <- length(p)
    TT <- length(NT) - 1
    mu_t <- rep(0, TT)

    for (i in 1:TT) {
        Nt <- NT[i:max(1, i-k+1)]
        mu_t[i]	<- sum(p[1:min(k, i)] * Nt)
    }

    mu_t <- R0 * mu_t
    LL <- sum(NT[-1] * log(mu_t)) - sum(mu_t)

	return(LL)
}
