#' Computes success rate using Wilson estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param ... further arguments passed to or from other methods
#' @return an estimated succes rate (%) as a numeric
#' @family success rate estimators
#' @rdname wilson
#' @export
#'
#'
wilson <- function(.success, ...) {
  UseMethod("wilson", .success)
}

#' @rdname wilson

#' @export
#'
wilson.default <-
  function(.success, .trials, ...){
    W <- 1.96 ^ 2 #z critical 95% confidence interval
    success <- (W / 2) + .success
    trials <- (.trials + W)
    out<- (success / trials)
    out
  }
