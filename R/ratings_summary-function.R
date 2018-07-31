#' Compute summary stats for ratings
#' @param .data  a vector, one-column dataframe, or a one-dimensional list of completion time values
#' @param ... further arguments passed to or from other methods
#' @return a dataframe of statistics
#' @family Data summaries time estimators
#' @include t_critical-function.R
#' @import tidyverse
#' @rdname ratings_summary
#' @export

ratings_summary <- function(.data, ...) {
  UseMethod("ratings_summary", .data)
}

#' @rdname ratings_summary

#' @export
#'
ratings_summary.default <-
  function(.data, ...){
  tcrit<-
    t_critical(.df=length(.data))
  std.err <- (sd(.data)/sqrt(length(.data)))
  margin.err <- tcrit*(sd(.data)/sqrt(length(.data)))
   data.frame(
     "Mean" = mean(.data),
     "StdDev" = sd(.data),
     "N" = length(.data),
     "StdErr" = std.err,
     "MargErr" = margin.err,
     "CI.hi" = mean(.data) + margin.err,
     "CI.lo" = mean(.data) - margin.err
   )
  }

#' @rdname ratings_summary
#' @param .name The name of the column with ratings data (Optional)
#' @export
#'
ratings_summary.data.frame <-
  function(.data, .name, ... ){
    .name <- enquo(.name)
    .data <-
      .data %>%
      dplyr::select(!! .name) %>%
     apply(MARGIN = 2, FUN = ratings_summary)
    .data
  }

