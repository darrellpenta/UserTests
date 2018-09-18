#' Computes estimated success rate with confidence intervals for small samples, following Sauro and Lewis (2012)
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param ... further arguments passed to or from other methods
#' @return a dataframe with stats
#' @family success rate estimators
#' @import tidyverse
#' @include df.success_rate.helper-function.R
#' @include wilson-function.R
#' @include laplace-function.R
#' @include mle-function.R
#' @include wald.ci-function.R
#' @rdname success_rate
#' @export
#'
#'
success_rate <- function(.success, ...) {
  UseMethod("success_rate", .success)
}

#' @rdname success_rate

#' @export
#'
success_rate.default <-
  function(.success, .trials, ...){
    p <- .success / .trials

    if(p > 1){
      return("STOP! Check your calculations; rate is greater than 100")
      stop()
    }
    else if (p < 0) {
      return("STOP! Check your calculations; rate is less than 0")
      stop()
    }
    else if(p == 0){
      p.out<-laplace(.success=.success, .trials = .trials)
      ci <-
        wald.ci(.success=.success, .trials=.trials, .Z = 1.64)
      out <-list("=0", "Laplace", p.out, list(0,ci[[2]]))
      out
    }

    else if (p == 1){
      p.out<-laplace(.success=.success, .trials = .trials)
      ci <-
        wald.ci(.success=.success, .trials=.trials, .Z = 1.64)
      out <-list("=1", "Laplace", p.out, list(ci[[1]],100))
      out
    }
    else if (p < .5 && p != 0) {
      p.out <-
        wilson(.success = .success, .trials = .trials)
      ci <-
        wald.ci(.success = .success, .trials = .trials)
      out <- list("<.5", "Wilson", p.out, ci)
      out
    }

    else if (p > .9 && p != 0) {
      p.out <-
        laplace(.success = .success, .trials = .trials, ...)
      ci <-
        wald.ci(.success = .success, .trials = .trials, ...)
      out <- list("<.9", "Laplace", p.out, ci)
      out
    }
    else {
      p.out <-
        mle(.success = .success, .trials = .trials, ...)
      ci <-
        wald.ci(.success = .success, .trials = .trials, ...)
      out <- list(".5<p<.9", "MLE", p.out, ci)
      out
    }
    return(
      data.frame(
        "successes" = .success,
        "trials" = .trials,
        "orig.succ.pct" = round(p * 100,2),
        "estimator" = out[[2]],
        "success.pct" = round(out[[3]] *100,2),
        "low.ci.pct" = ifelse(out[[4]][[1]] == 0,0,round(out[[4]][[1]]*100,2)),
        "hi.ci.pct" = ifelse(out[[4]][[2]] == 100,100,round(out[[4]][[2]] * 100,2)),
        stringsAsFactors = FALSE
      )
    )
  }

#' @rdname success_rate
#' @import dplyr
#' @export
#'
success_rate.data.frame <- function(.success, ...){

  if(ncol(.success)==3){

    out <-
      .success %>%
      dplyr::group_by(Task) %>%
      dplyr::mutate_at(vars("Success"), funs(as.numeric(.))) %>%
      dplyr::summarise_at(vars("Success"), funs(trials = n(), success=sum(.))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Task) %>%
      dplyr::do(df.success_rate.helper(.))
    out
  }
else if(ncol(.success)==4){

    out <-
      .success %>%
      dplyr::group_by(Task, Group) %>%
      dplyr::mutate_at(vars("Success"), funs(as.numeric(.))) %>%
      dplyr::summarise_at(vars("Success"), funs(trials = n(), success=sum(.))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Task, Group) %>%
      dplyr::do(df.success_rate.helper(.))
    out
  }
else{
    stop("You have too many columns in your data set.")
  }
}
