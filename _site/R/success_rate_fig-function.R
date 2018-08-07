#' Creates bar graph of success rates
#'
#' @param .data The results (as a data frame) from \code{success_rate} function
#' @param ... further arguments passed to or from other methods
#' @param legend_lab Label for the legend (when groups are compared)
#' @return a ggplot figure
#' @family success rate estimators
#' @import tidyverse
#' @rdname success_rate_fig
#' @export
#'
success_rate_fig <- function(.data, ...) {
  UseMethod("success_rate_fig", .data)
}

#' @rdname success_rate_fig
#' @param xlabel Label for the x axis
#' @param ylabel Label for the y axis
#' @param legend_lab Label for the legend (when groups are compared)
#' @export
#'
success_rate_fig.default <- function(.data,..., xlabel="Objective Success (%)", ylabel="Task", legend_lab="Group"){
if(ncol(.data)==8){

  x<-
    ggplot(.data,mapping = aes(x=Task, y = success.pct, fill=success.pct)) +
    layer(geom ="bar", stat ="identity", position = position_dodge(0.3)) +
    guides(fill=FALSE) +
    geom_errorbar(aes(ymax =  hi.ci.pct, ymin = low.ci.pct),
                  width=.25,
                  size=0.5,
                  position=position_dodge(0.3)) +
    theme_classic() +
    theme(text = element_text(size=18.5)) +
    ylab(ylabel) +
    xlab(xlabel) +
    theme(axis.title.y = element_text(vjust = 1.5)) +
    theme(axis.title.x = element_text(vjust = -1), axis.text.x = element_text(size = 18, face="bold")) +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    scale_colour_brewer(palette = "Set1")
  x

}
  else if(ncol(.data)==9){
  x<-
  ggplot(.data,mapping = aes(x=Task, y = success.pct, fill=Group)) +
    layer(geom ="bar", stat ="identity", position = position_dodge()) +
    geom_errorbar(aes(ymax =  hi.ci.pct, ymin = low.ci.pct),
                  position = position_dodge(.9),width = 0.2)  +
    theme_classic() +
    theme(text = element_text(size=18.5)) +
    ylab(ylabel) +
    xlab(xlabel) +
    theme(axis.title.y = element_text(vjust = 1.5)) +
    theme(axis.title.x = element_text(vjust = -1), axis.text.x = element_text(size = 18, face="bold")) +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    scale_fill_manual(values=c('lightgray','#6FABD2'), name=legend_lab)
  x

}
}
