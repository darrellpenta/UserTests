#' Computes estimated success rate with confidence intervals
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param ... further arguments passed to or from other methods
#' @return a dataframe with stats
#' @family success rate estimators
#' @include wilson-function.R
#' @include laplace-function.R
#' @include mle-function.R
#' @include wald.ci-function.R
#' @rdname completion
#' @export
#'
#'
completion <- function(.success, ...) {
  UseMethod("completion", .success)
}

#' @rdname completion

#' @export
#'
completion.default <-
  function(.success, .trials, ...){
    p <- .success / .trials

    if(p > 1 | p < 0){
      return("STOP! Check your calculations; rate is either less than 0 or greater than 100")
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
        "Successes" = .success,
        "Total Trials" = .trials,
        "Orig.Success.pct" = round(p * 100,2),
        "Estimate" = out[[2]],
        "Success.pct" = round(out[[3]] *100,2),
        "Low.ci.pct" = ifelse(out[[4]][[1]] == 0,0,round(out[[4]][[1]]*100,2)),
        "High.ci.pct" = ifelse(out[[4]][[2]] == 100,100,round(out[[4]][[2]] * 100,2))
      )
    )
  }

#' @rdname completion

#' @export
#'
completion.data.frame <-
  function(.success, ...){

    .success <-
      .success %>%
      group_by(Task) %>%
      summarise_at(vars("Success"), funs(trials = n(), success=sum(.))) %>%
      ungroup()

    df_completion <-function(x){

      success <- x[[3]][1]
      trials <- x[[2]][1]
      out <- data.frame(completion(.success=success, .trials=trials))
      out
    }

    out<- .success %>% group_by(Task) %>% do(df_completion(.)) %>% mutate_if(is.factor, as.character)
    out
  }
