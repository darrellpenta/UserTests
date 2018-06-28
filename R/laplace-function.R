#' Computes success rate using Laplace estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param ... further arguments passed to or from other methods
#' @return an estimated succes rate (%) as a numeric
#' @family success rate estimators
#' @rdname laplace
#' @export
#'
#'
laplace <- function(.success, ...) {
  UseMethod("laplace", .success)
}

#' @rdname laplace

#' @export
#'
laplace.default <-
  function(.success, .trials, ...){
    success <- .success + 1
    trials <- .trials + 2
    out<-
      (success / trials)
    out
  }
