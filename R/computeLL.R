#' WP method background function computeLL
#'
#' This is a background/internal function called by \code{WP}.  It computes the log-likelihood.
#'
#' @param NT vector of case counts
#' @param p discretized version of the serial distribution
#' @param R0 basic reproductive ratio
#' @return The function returns the variable \code{LL} which is the log-likelihood at the input variables and parameters.
#'
#' @export
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
