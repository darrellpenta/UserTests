#' Computes the geometric mean for small sample size (n<=25) completion times
#' @param .data  a vector, one-column dataframe, or a one-dimensional list of completion time values
#' @param ... further arguments passed to or from other methods
#' @return a geometric mean
#' @family Completion time estimators
#' @rdname geo_mean
#' @export
#'

geo_mean <- function(.data, ...) {
  UseMethod("geo_mean", .data)
}

#' @rdname geo_mean

#' @export
#'
geo_mean.default <-
  function(.data, ...){
    if(length(.data)>25){
      return("Are you sure you want the Geometric Mean? You have more than 25 data points.")
      stop()
    }else{
    exp(mean(log(.data)))
  }
  }

#' @rdname geo_mean

#' @export
#'
geo_mean.data.frame <-
  function(.data, ...){
  if(ncol(.data)>1){
    return("Are you sure you want the Geometric Mean? You have more than 25 data points.")
    stop()
  }
    else{
      exp(mean(apply(.data, 2, log)))
    }
  }


#' @rdname geo_mean

#' @export
#'
geo_mean.list<-
  function(.data, ...){
    if(length(.data[[1]])>25){
      return("Are you sure you want the Geometric Mean? You have more than 25 data points.")
      stop()
    }
    else{
  exp(mean(sapply(dd[[1]],FUN = log, simplify = TRUE)))
    }
  }

