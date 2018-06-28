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
df_completion <-function(.data, ...){
  success <- .data[1, which(colnames(.data))=="success"]
  trials <- .data[1, which(colnames(.data))=="trials"]
  out <- data.frame(completion(.success=success, .trials=trials))
  out
}
