#' ID method
#'
#' This function implements a least squares estimation method of R0 due to Fisman et al. (PloS One, 2013).
#' See details for implementation notes.
#'
#' The method is based on a straightforward incidence decay model. The estimate of R0 is the value which
#' minimizes the sum of squares between observed case counts and cases counts 'expected' under the model.
#'
#' This method is based on an approximation of the SIR model, which is most valid at the beginning of an epidemic.
#' The method assumes that the mean of the serial distribution (sometimes called the serial interval) is known.
#' The final estimate can be quite sensitive to this value, so sensitivity testing is strongly recommended.
#' Users should be careful about units of time (e.g., are counts observed daily or weekly?) when implementing.
#'
#' @param NT Vector of case counts.
#' @param mu Mean of the serial distribution. This needs to match case counts in time units. For example, if case counts
#'           are weekly and the serial distribution has a mean of seven days, then \code{mu} should be set to one If case
#'           counts are daily and the serial distribution has a mean of seven days, then \code{mu} should be set to seven.
#'
#' @return \code{ID} returns a single value, the estimate of R0.
#'
#' @examples
#' ## ===================================================== ##
#' ## Illustrate on weekly data                             ##
#' ## ===================================================== ##
#'
#' NT <- c(1, 4, 10, 5, 3, 4, 19, 3, 3, 14, 4)
#' ## obtain Rhat when serial distribution has mean of five days
#' ID(NT=NT, mu=5/7)
#' ## obtain Rhat when serial distribution has mean of three days
#' ID(NT=NT, mu=3/7)
#'
#' ## ========================================================= ##
#' ## Compute Rhat using only the first five weeks of data      ##
#' ## ========================================================= ##
#'
#' ID(NT=NT[1:5], mu=5/7) # serial distribution has mean of five days
#'
#' @export
ID <- function(NT, mu) {
    NT <- as.numeric(NT)
    TT <- length(NT)
    s <- (1:TT) / mu
    y <- log(NT) / s

    R0_ID <- exp(sum(y) / TT)

    return(R0_ID)
}
