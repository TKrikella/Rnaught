source("computeLL.R")
source("WP_known.R")

#' WP method background function WP_unknown
#'
#' This is a background/internal function called by \code{WP}. It computes the maximum likelihood estimator
#' of R0 assuming that the serial distribution is unknown but comes from a discretized gamma distribution.
#' The function then implements a simple grid search algorithm to obtain the maximum likelihood estimator
#' of R0 as well as the gamma parameters.
#'
#' @param NT vector of case counts
#' @param B length of grid for shape and scale (grid search parameter)
#' @param shape.max maximum shape value (grid search parameter)
#' @param scale.max maximum scale value (grid search parameter)
#' @param tol cutoff value for cumulative distribution function of the serial distribution, defaults to 0.999
#'
#' @return The function returns \code{Rhat}, the maximum likelihood estimator of R0, as well as the maximum
#'         likelihood estimator of the discretized serial distribution given by \code{p} (the probability mass
#'         function) and \code{range.max} (the distribution has support on the integers one to \code{range.max}).
#'         The function also returns \code{resLL} (all values of the log-likelihood) at \code{shape} (grid for
#'         shape parameter) and at \code{scale} (grid for scale parameter), as well as \code{resR0} (the full
#'         vector of maximum likelihood estimators), \code{JJ} (the locations for the likelihood for these), and
#'         \code{J0} (the location for the maximum likelihood estimator \code{Rhat}).  If \code{JJ} and \code{J0}
#'         are not the same, this means that the maximum likelihood estimator is not unique. 
#'
#' @export
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
