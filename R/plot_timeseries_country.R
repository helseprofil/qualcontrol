#' @title plot_timeseries_country
#' @description
#' Plots country-level time series across all dimensions except GEO and AAR.
#' Useful to detect classification errors or other time series anomalies.
#' @param cube data file
#' @param save Should the plot be saved in the TimeSeries_country folder? Default = TRUE.
#' If FALSE, the plot is just printed in the console.
#' @return plot
#' @export
plot_timeseries_country <- function(dt = newcube,
                                    save = TRUE){
  d <- data.table::copy(dt[GEO == 0])
  colinfo <- identify_coltypes(d)
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  plotdims <- grep("^GEO$|^AAR$", colinfo$dims.new, invert = T, value = T)
  plotvals <- c(grep("^RATE.n|^SPVFLAGG$|TELLER|NEVNER", colinfo$vals.new, invert = T, value = T),
                select_teller_pri(colinfo$vals.new),
                select_nevner_pri(colinfo$vals.new))
  plotrows = ceiling(length(plotvals)/2)
  d <- d[, c(..colinfo[["dims.new"]], ..plotvals)]
  data.table::setkeyv(d, colinfo$dims.new)
  d[, let(AARx = as.numeric(sub("_\\d*", "", AAR)))]
  d[, (plotvals) := lapply(.SD, as.numeric), .SDcols = plotvals]
  d[, (plotdims) := lapply(.SD, as.factor), .SDcols = plotdims]

  savepath <- get_plotsavefolder(cubename, "TimeSeries_country")
  if(save) archive_old_files(savepath, cubefile)

  for(dim in c("Total", plotdims)){
    if(dim == "Total") plotdata <- aggregate_cube_multi(d, plotdims)
    if(dim != "Total") plotdata <- aggregate_cube_multi(d, grep(dim, plotdims, invert = T, value = T))
    plotdata <- data.table::melt(plotdata,
                                 measure.vars = plotvals,
                                 variable.name = "PARAMETER",
                                 value.name = "yvalue")
    plotdata[, let(Total = "total")]
    plot <- plot_timeseries_country_plotfun(plotdata, dim)
    plot_timeseries_country_savefun(plot, savepath, dim, cubefile, plotrows, save = save)
  }
}

#' @title plot_timeseries_country_plotfun
#' @description
#' Plotting function for [plot_timeseries_country()]
#' @keywords internal
#' @noRd
plot_timeseries_country_plotfun <- function(plotdata,
                                            dim){
  nrow_legend <- ceiling(length(unique(plotdata[[dim]]))/3)
  plot <- ggplot2::ggplot(plotdata, ggplot2::aes(AARx,
                                                 yvalue,
                                                 color = .data[[dim]],
                                                 group = .data[[dim]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~as.character(PARAMETER), ncol = 2, scales = "free_y") +
    ggplot2::labs(x = "Year", y = NULL, title = dim) +
    ggplot2::scale_x_continuous(breaks = seq(min(plotdata$AARx),max(plotdata$AARx), by = 1),
                                labels = sort(unique(plotdata$AAR)),
                                expand = ggplot2::expansion(add = 0.2)) +
    theme_qc() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 8),
                   panel.spacing = ggplot2::unit(0.5, "cm"))  +
    ggh4x::force_panelsizes(rows = ggplot2::unit(5, "cm"),
                            cols = ggplot2::unit(7, "cm"))

  suppresslegend <- nrow_legend > 3
  if(suppresslegend){
    plot <- plot + ggplot2::guides(color = "none")
  }
  if(!suppresslegend){
    plot <- plot +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL,
                                                  nrow = nrow_legend,
                                                  byrow = TRUE))
  }
}

#' @title plot_timeseries_country_savefun
#' @description
#' Save function for [plot_timeseries_country()]
#' @keywords internal
#' @noRd
plot_timeseries_country_savefun <- function(plot,
                                            savepath,
                                            dim,
                                            cubefile,
                                            plotrows,
                                            save = TRUE){

  savename <- paste0(cubefile, "_", dim, ".png")

  if(save){
    ggplot2::ggsave(file.path(savepath, savename),
                    plot,
                    width = 20,
                    height =  8 + (6*plotrows),
                    units = "cm")
  }
}

