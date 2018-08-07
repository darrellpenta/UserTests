#' A convenience function based on ``qt()`` that returns a t-critical value
#' @param .alpha the level of significance, defaulting to 0.05
#' @param ... further arguments passed to or from other methods
#' @import tidyverse
#' @import lazyeval
#' @return a t-critical value
#' @family Confidence Intervals
#' @rdname t_critical
#' @export
#'
#'
#'

t_critical<-function(.alpha = .05, ...){
  UseMethod("t_critical", .alpha)
}

#' @param .df the degrees of freedom
#' @param .form an optional formula, passed to ``qt()``, to specify how the critical T value should be computed
#' @rdname t_critical
#' @export
#'

t_critical.default <- function(.alpha = .05,..., .df, .form=NULL ){
  if(hasArg(".form")){
    as<- paste0("~qt(", lazyeval::f_eval(.form), ",","df=", .df,")")
    as<- formula(as)
    lazyeval::f_eval(as)
  } else if(missing(.alpha)==TRUE){
    qt(1-.05/2, df=.df)
  } else{
    qt(1-.alpha/2, df=.df)
  }

}



