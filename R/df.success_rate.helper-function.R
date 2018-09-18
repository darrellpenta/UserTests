#' Prepare a data frame for analysis by success_rate function
#'
#' @param .data  a data frame
#' @param ... further arguments passed to or from other methods
#' @return a dataframe with stats
#' @family success rate estimators
#' @rdname df.success_rate.helper
#' @export
#'
#'
df.success_rate.helper <-function(.data,...){

  if(ncol(.data)==3){
  success <- .data[[3]][1]
  trials <- .data[[2]][1]
  out <- data.frame(success_rate(.success=success, .trials=trials), stringsAsFactors = FALSE)
  out
  }
  else if(ncol(.data)==4) {
    success <- .data[[4]][1]
    trials <- .data[[3]][1]
    out <- data.frame(success_rate(.success=success, .trials=trials), stringsAsFactors = FALSE)
    out
  }
  else{
    stop("You have too many columns in your data set.")
  }
}
