#' Computes confidence intervals using adjusted Wald (Agresti & Coull, 1998)
#'
#'
#' @param .success  the proportion of successes
#' @param .trials the total number of trials
#' @param .Z z score for confidence limit
#' @param ... further arguments passed to or from other methods
#' @return a list with the lower and upper confidence limits (%) as numerics
#' @family success rate estimators
#' @rdname wald.ci
#' @export
#'
#'
wald.ci <- function(.success, ...) {
  UseMethod("wald.ci", .success)
}

#' @rdname wald.ci

#' @export
#'
wald.ci.default <-
  function(.success, .trials, .Z = 1.96, ...){
    W <- .Z ^ 2 #z critical 95% confidence interval
    success <- (W / 2) + .success
    trials <- (.trials + W)
    p <- success / trials
    nn <- p * (1 - p)
    est <- sqrt((nn / trials))
    z <- .Z * est
    p.hi <- p + z
    p.lo <- p - z
    return(list(p.lo, p.hi))
  }
