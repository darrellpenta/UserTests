#' Creates bar graph of completion rates
#'
#' @param .data The results (as a data frame) from \code{Completion} function
#' @param ... further arguments passed to or from other methods
#' @return a ggplot figure
#' @family success rate estimators
#' @rdname comp_figure
#' @export
#'
comp_figure <- function(.data, ...) {
  UseMethod("completion", .data)
}

#' @rdname comp_figure

#' @export
#'
comp_figure.default <- function(.data,...){
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
    ylab("Objective Success (%)") +
    xlab("Task") +
    theme(axis.title.y = element_text(vjust = 1.5)) +
    theme(axis.title.x = element_text(vjust = -1), axis.text.x = element_text(size = 18, face="bold")) +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    scale_colour_brewer(palette = "Set1") 
  x
  
}
  else if(ncol(.data)==9){
  X<-
  ggplot(.data,mapping = aes(x=Task, y = success.pct, fill=Group)) +
    layer(geom ="bar", stat ="identity", position = position_dodge()) +
    geom_errorbar(aes(ymax =  hi.ci.pct, ymin = low.ci.pct),
                  position = position_dodge(.9),width = 0.2)  + 
    theme_classic() +
    theme(text = element_text(size=18.5)) +
    ylab("Objective Success (%)") +
    xlab("Task") +
    theme(axis.title.y = element_text(vjust = 1.5)) +
    theme(axis.title.x = element_text(vjust = -1), axis.text.x = element_text(size = 18, face="bold")) +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
    scale_fill_manual(values=c('lightgray','#6FABD2'), name="EMM Tool")
  x

}
}
