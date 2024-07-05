#' @title Quality control default plot theme
#' @export
theme_qc <- function(){
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 panel.grid.minor = element_blank(),
                 text = element_text(color = "black", size = 14),
                 plot.margin = margin(t = 1, b = 1, r = 1, unit = "cm"))
}
