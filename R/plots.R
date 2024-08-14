#' @title plot_timeseries_country
#' @description
#' Plots country-level time series across all dimensions except GEO and AAR.
#' Useful to detect classification errors or other time series anomalies.
#' @param cube data file
#' @return list
#' @export
plot_timeseries_country <- function(cube){
  d <- data.table::copy(cube[GEO == 0])
  colinfo <- identify_coltypes(d)
  plotdims <- grep("^GEO$|^AAR$", colinfo$dims.new, invert = T, value = T)
  plotvals <- c(grep("^RATE.n|^SPVFLAGG$|TELLER|NEVNER", colinfo$vals.new, invert = T, value = T),
                select_teller_pri(colinfo$vals.new),
                select_nevner_pri(colinfo$vals.new))
  d <- d[, mget(c(colinfo$dims.new, plotvals))]
  data.table::setkeyv(d, colinfo$dims.new)
  d[, let(AARx = as.numeric(sub("_\\d*", "", AAR)))]
  d[, (plotvals) := lapply(.SD, as.numeric), .SDcols = plotvals]
  d[, (plotdims) := lapply(.SD, as.factor), .SDcols = plotdims]
  d <- data.table::melt(d,
                        measure.vars = plotvals,
                        variable.name = "PARAMETER",
                        value.name = "yvalue")
  plots <- list()
  for(dim in plotdims){
    plotdata <- aggregate_cube_multi(d, grep(dim, plotdims, invert = T, value = T))
    plots[[dim]] <- plot_timeseries_country_plotfun(plotdata, dim)
  }
  return(list(plots = plots,
              plotheight = 2+2*ceiling(length(plotvals))))
}

#' @title plot_timeseries_country_plotfun
#' @description
#' Plotting function for [plot_timeseries_country()]
#' @keywords internal
#' @noRd
plot_timeseries_country_plotfun <- function(plotdata,dim){
  nrow_legend <- ceiling(length(unique(plotdata[[dim]]))/3)
  plot <- ggplot2::ggplot(plotdata, aes(AARx,
                                        yvalue,
                                        color = .data[[dim]],
                                        group = .data[[dim]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~PARAMETER, ncol = 2, scales = "free_y") +
    ggplot2::labs(x = "Year", y = NULL, title = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(min(d$AARx),max(d$AARx),by = 1),
                                labels = sort(unique(d$AAR)),
                                expand = expansion(add = 0.2)) +
    theme_qc() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
                   panel.spacing = unit(0.5, "cm"))  +
    ggh4x::force_panelsizes(rows = unit(4, "cm"))

  suppresslegend <- nrow_legend > 3
  if(suppresslegend){
    plot <- plot + guides(color = "none")
  }
  if(!suppresslegend){
    plot <-  plot + guides(color = guide_legend(title = NULL,
                                                nrow = nrow_legend,
                                                byrow = TRUE))
  }
  return(plot)
}
