#' Computes success rate using Maximum Liklihood estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param ... further arguments passed to or from other methods
#' @return an estimated succes rate (%) as a numeric
#' @family success rate estimators
#' @rdname mle
#' @export
#'
#'
mle <- function(.success, ...) {
  UseMethod("mle", .success)
}

#' @rdname mle

#' @export
#'
mle.default <-
  function(.success, .trials, ...){
    out<-
      (.success / .trials)
    out
  }
