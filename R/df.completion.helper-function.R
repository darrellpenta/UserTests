#' Prepared a data frame for analysis by completion function
#'
#' @param .data  a data frame
#' @param ... further arguments passed to or from other methods
#' @return a dataframe with stats
#' @family success rate estimators
#' @rdname df.completion.helper
#' @export
#'
#'
df.completion.helper <-function(.data,...){

  if(ncol(.data)==3){
  success <- .data[[3]][1]
  trials <- .data[[2]][1]
  out <- data.frame(completion(.success=success, .trials=trials), stringsAsFactors = FALSE)
  out
  }
  else if(ncol(.data)==4) {
    success <- .data[[4]][1]
    trials <- .data[[3]][1]
    out <- data.frame(completion(.success=success, .trials=trials), stringsAsFactors = FALSE)
    out
  }
  else{
    stop("You have too many columns in your data set.")
  }
}
