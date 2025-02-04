#' @title Quality control default plot theme
#' @export
theme_qc <- function(){
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top",
                 panel.grid.minor = ggplot2::element_blank(),
                 text = ggplot2::element_text(color = "black", size = 20),
                 strip.text = ggplot2::element_text(hjust = 0),
                 plot.margin = ggplot2::margin(t = 1, b = 1, r = 1, l = 1, unit = "cm"))
}

#' @title tab_output
#' @keywords internal
#' @description
#' Formats output tables using [DT::datatable()].By default, all
#' columns is searchable unless mentioned as a nosearchcolumn.
#'
#' @param table output table
#' @param nosearchcolumns vector of columns to disable searching
#' @param filter Where to put the filter, defaults to "top", but can be "bottom" or "none"
#' @param rownames Should rownames be printed? Defaults to FALSE
#' @param dom Control the table's layout (e.g., 'lfrtip' for length, filter, records,
#' table, info, pagination). Defaults to "ltpi"
#' @param scrollX Enable vertical scrolling, defaults to TRUE
#'
#' @return datatable
tab_output <- function(table,
                       nosearchcolumns = NULL,
                       filter = "top",
                       rownames = FALSE,
                       dom = "ltpi",
                       scrollX = TRUE){
  DT::datatable(
    table,
    filter = filter,
    rownames = rownames,
    options = list(
      columnDefs = list(list(targets = nosearchcolumns,
                             searchable = FALSE)),
      dom = dom,
      scrollX = scrollX
    )
  )
}
