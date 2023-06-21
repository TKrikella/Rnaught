#' WP method background function WP_known
#'
#' This is a background/internal function called by \code{WP}. It computes the maximum
#' likelihood estimator of R0 assuming that the serial distribution is known and finite discrete.
#'
#' @param NT vector of case counts
#' @param p discretized version of the serial distribution
#' @return The function returns \code{Rhat}, the maximum likelihood estimator of R0.
#'
#' @export
WP_known <- function(NT, p) {
    k <- length(p)
    TT <- length(NT) - 1
	mu_t <- rep(0, TT)

    for (i in 1:TT) {
        Nt <- NT[i:max(1, i-k+1)]
        mu_t[i]	<- sum(p[1:min(k, i)] * Nt)
    }

    Rhat <- sum(NT[-1]) / sum(mu_t)
	return(list(Rhat=Rhat))
}
