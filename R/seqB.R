#' seqB method
#'
#' This function implements a sequential Bayesian estimation method of R0 due to Bettencourt and Riberio (PloS One, 2008).
#' See details for important implementation notes.
#'
#' The method sets a uniform prior distribution on R0 with possible values between zero and \code{kappa}, discretized to a fine grid.
#' The distribution of R0 is then updated sequentially, with one update for each new case count observation.
#' The final estimate of R0 is \code{Rhat}, the mean of the (last) posterior distribution.
#' The prior distribution is the initial belief of the distribution of R0; which in this implementation is the uninformative uniform
#' distribution with values between zero and \code{kappa}. Users can change the value of /code{kappa} only (i.e., the prior distribution
#' cannot be changed from the uniform).  As more case counts are observed, the influence of the prior distribution should lessen on
#' the final estimate \code{Rhat}.
#'
#' This method is based on an approximation of the SIR model, which is most valid at the beginning of an epidemic. The method assumes
#' that the mean of the serial distribution (sometimes called the serial interval) is known. The final estimate can be quite sensitive
#' to this value, so sensitivity testing is strongly recommended. Users should be careful about units of time (e.g., are counts observed
#' daily or weekly?) when implementing.  
#'
#' Our code has been modified to provide an estimate even if case counts equal to zero are present in some time intervals. This is done
#' by grouping the counts over such periods of time. Without grouping, and in the presence of zero counts, no estimate can be provided.
#'
#' @param NT Vector of case counts.
#' @param mu Mean of the serial distribution. This needs to match case counts in time units. For example, if case counts
#'           are weekly and the serial distribution has a mean of seven days, then \code{mu} should be set to one. If case
#'           counts are daily and the serial distribution has a mean of seven days, then \code{mu} should be set to seven.
#' @param kappa Largest possible value of uniform prior (defaults to 20). This describes the prior belief on ranges of R0,
#'              and should be set to a higher value if R0 is believed to be larger.  
#'
#' @return \code{secB} returns a list containing the following components: \code{Rhat} is the estimate of R0 (the posterior mean),
#'         \code{posterior} is the posterior distribution of R0 from which alternate estimates can be obtained (see examples),
#'         and \code{group} is an indicator variable (if \code{group=TRUE}, zero values of NT were input and grouping was done 
#'         to obtain \code{Rhat}). The variable \code{posterior} is returned as a list made up of \code{supp} (the support of
#'         the distribution) and \code{pmf} (the probability mass function).
#'
#' @examples
#' ## ===================================================== ##
#' ## Illustrate on weekly data                             ##
#' ## ===================================================== ##
#'
#' NT <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#' ## obtain Rhat when serial distribution has mean of five days
#' res1 <- seqB(NT=NT, mu=5/7)
#' res1$Rhat
#' ## obtain Rhat when serial distribution has mean of three days
#' res2	<- seqB(NT=NT, mu=3/7)
#' res2$Rhat
#'
#' ## ============================================================= ##
#' ## Compute posterior mode instead of posterior mean and plot     ##
#' ## ============================================================= ##
#'
#' Rpost <-	res1$posterior
#' loc <- which(Rpost$pmf == max(Rpost$pmf))
#' Rpost$supp[loc] # posterior mode
#' res1$Rhat # compare with posterior mean
#'
#' par(mfrow=c(2, 1), mar=c(2, 2, 1, 1))
#' plot(Rpost$supp, Rpost$pmf, col="black", type="l", xlab="", ylab="")
#' abline(h=1/(20/0.01+1), col="red")
#' abline(v=res1$Rhat, col="blue")
#' abline(v=Rpost$supp[loc], col="purple")
#' legend("topright", legend=c("prior", "posterior", "posterior mean (Rhat)", "posterior mode"),
#'        col=c("red", "black", "blue", "purple"), lty=1)
#' plot(Rpost$supp, Rpost$pmf, col="black", type="l", xlim=c(0.5, 1.5), xlab="", ylab="")
#' abline(h=1/(20/0.01+1), col="red")
#' abline(v=res1$Rhat, col="blue")
#' abline(v=Rpost$supp[loc], col="purple")
#' legend("topright", legend=c("prior", "posterior", "posterior mean (Rhat)", "posterior mode"),
#'        col=c("red", "black", "blue", "purple"), lty=1)
#'
#' ## ========================================================= ##
#' ## Compute Rhat using only the first five weeks of data      ##
#' ## ========================================================= ##
#' 
#' res3 <- seqB(NT=NT[1:5], mu=5/7)	# serial distribution has mean of five days
#' res3$Rhat
#'
#' @export
seqB <- function(NT, mu, kappa=20) {	
    if (length(NT) < 2)
        print("Warning: length of NT should be at least two.")
    else {
        if (min(NT) > 0) {
            times <- 1:length(NT)
            tau <- diff(times)
        }
	    group <- FALSE
        if (min(NT) == 0) {
            times <- which(NT > 0)
            NT <- NT[times]
            tau <- diff(times)
            group <- TRUE
        }

        R <- seq(0, kappa, 0.01)
        prior0 <- rep(1, kappa / 0.01 + 1)
        prior0 <- prior0 / sum(prior0)
        k <- length(NT) - 1
        R0.post <- matrix(0, nrow=k, ncol=length(R))
        prior <- prior0
        posterior <- seq(0, length(prior0))
        gamma <- 1 / mu

        for (i in 1:k) {
            mm1 <- NT[i]
            mm2 <- NT[i+1]
            lambda <- tau[i] * gamma * (R - 1)
            lambda <- log(mm1) + lambda
            loglik <- mm2 * lambda - exp(lambda)
            maxll <- max(loglik)
            const <- 0

            if (maxll > 700)
                const <- maxll - 700

            loglik <- loglik-const
            posterior <- exp(loglik) * prior
            posterior <- posterior / sum(posterior)
            prior <- posterior
        }

        Rhat <- sum(R * posterior)

        return(list(Rhat=Rhat, posterior=list(supp=R, pmf=posterior), group=group))
    }	
}
