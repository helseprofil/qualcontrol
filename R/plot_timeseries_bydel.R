#' @title plot_timeseries_bydel
#' @description
#' Generate timeseries plots for bydel, with weighted
#' @param dt flagged data file, default to newcube_flag
#' @export
plot_timeseries_bydel <- function(dt = newcube_flag,
                                  save = TRUE){
  if(nrow(dt[GEOniv == "B"]) == 0){
    cat("No data on bydel, no check performed")
    return(invisible(NULL))
  }
  d <- data.table::copy(dt)
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  colinfo <- identify_coltypes(d)
  plotvalue <- select_outlier_pri(d, colinfo = colinfo)
  d <- d[(GEO %in% c(301, 1103, 4601, 5001) | GEOniv == "B") & !is.na(get(plotvalue))]
  contains_bydel <- d[GEOniv == "B" & SPVFLAGG == 0, unique(AAR)]
  d <- d[AAR %in% contains_bydel]
  add_kommune(d)
  bycols <- c("KOMMUNE", grep("^GEO$", colinfo$dims.new, invert = T, value = T))

  panels <- grep("^KOMMUNE$|^AAR$", bycols, invert = T, value = T)
  filedims <- get_plot_subset(d, panels, maxpanels = 5)
  if(length(filedims > 0)) panels <- panels[panels %notin% filedims]
  filter <- get_plot_filter(d, filedims)
  d[, let(allpanels = "alle")]
  if(length(panels > 0)) d[, allpanels := interaction(mget(panels))]

  trends <- plot_timeseries_bydel_trendlines(d, bycols, filedims, plotvalue)

  # Generate global plot elements
  plotargs <- list()
  plotargs$plotvalue <- plotvalue
  plotargs$title <- paste0("File: ", attributes(dt)$Filename, ", Plotting date: ", Sys.Date())
  plotargs$subtitle <- paste0("Variable plotted: ", plotvalue)
  plotargs$allplotdims <- get_all_combinations(d, c("KOMMUNE", "allpanels"))
  plotargs$anyrows <- ifelse(length(panels) > 0, 1, 0)
  rows <- nrow(plotargs$allplotdims[, .N, by = allpanels])
  savepath <- get_plotsavefolder(cubename, "TimeSeries_bydel")
  if(save) archive_old_files(savepath, cubename)

  for(i in filter){
    cat("\nSaving file", which(filter == i), "/", length(filter))
    plotdata <- d[eval(parse(text = i))]
    trenddata <- trends[eval(parse(text = i))]

    if(nrow(plotdata)>0){
    suffix <- get_multifile_plot_suffix(plotdata, filedims)
    plotargs$subtitle_full <- plotargs$subtitle
      for(i in filedims){
        plotargs$subtitle_full <- paste0(plotargs$subtitle_full, "\n", i, ": ", unique(plotdata[[i]]))
      }
    plot <- plot_timeseries_bydel_plotfun(plotdata, trenddata, plotargs)
    if(save) plot_timeseries_bydel_savefun(plot, savepath, cubefile, suffix, rows)
    print(plot)
    }
  }
}

#' @title plot_boxplot_plotfun
#' @description
#' Plotting function for [qualcontrol::plot_timeseries_bydel()]
#' @keywords internal
#' @noRd
#' @param pd plotdata
#' @param td trenddata
#' @param plotargs list of plot arguments
plot_timeseries_bydel_plotfun <- function(pd,
                                          td,
                                          plotargs){

  plot <- ggplot2::ggplot(plotargs$allplotdims) +
    ggplot2::facet_grid(cols = ggplot2::vars(KOMMUNE),
                        rows = ggplot2::vars(allpanels),
                        switch = "y",
                        scales = "free_y") +
    ggplot2::labs(title = plotargs$title,
                  subtitle = plotargs$subtitle_full,
                  y = plotargs$plotvalue) +
    ggplot2::geom_line(data = td,
                       ggplot2::aes(x = AAR, y = y, color = type, group = type),
                       linewidth = 1.5) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::geom_line(data = pd,
                       ggplot2::aes(x = AAR, y = get(plotargs$plotvalue), group = GEO), linetype = 2) +
    ggplot2::geom_point(data = pd,
                        ggplot2::aes(x = AAR, y = get(plotargs$plotvalue)),
               size = 3, shape = 1) +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    theme_qc() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))

  if(plotargs$anyrows == 0){
    plot <- plot +
      ggplot2::theme(strip.background.y = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_blank())
  }

  return(plot)
}

#' @title plot_boxplot_savefun
#' @description
#' Plotting function for [qualcontrol::plot_timeseries_bydel()]
#' @keywords internal
#' @noRd
#' @param plot plot
#' @param cubename cubename
#' @param cubefile cube filename
#' @param suffix suffix
#' @param rows number of rows
plot_timeseries_bydel_savefun <- function(plot,
                                          savepath,
                                          cubefile,
                                          suffix,
                                          rows){

  savename <- paste0(cubefile, "_", suffix, ".png")
  height = rows*6 + 12

  ggplot2::ggsave(file.path(savepath, savename),
                  plot,
                  dpi = 300,
                  width = 37,
                  height = height,
                  units = "cm")
}

## ---- HELPER FUNCTIONS ----

#' @title plot_timeseries_bydel_trendlines
#' @keywords internal
#' @noRd
plot_timeseries_bydel_trendlines <- function(dt,
                                             bycols,
                                             filedims,
                                             plotvalue){

  bd <- dt[GEOniv == "B"][, n_geo := .N, by = c("GEO", filedims, "allpanels")]
  kd <- dt[GEOniv == "K" & AAR %in% unique(bd$AAR)][, n_geo := .N, by = c("GEO", filedims, "allpanels")]

  bg <- collapse::GRP(bd, c(bycols, "allpanels"))
  bw <- bd$WEIGHTS
  bydel <- collapse::fmutate(bg[["groups"]],
                          y = collapse::fmean(bd[[plotvalue]], w = bw, g = bg),
                          type = "Vektet bydel")

  kg <- collapse::GRP(kd, c(bycols, "allpanels"))
  kw <- kd$WEIGHTS
  kommune <- collapse::fmutate(kg[["groups"]],
                          y = collapse::fmean(kd[[plotvalue]], w = kw, g = kg),
                          type = "Kommune")

  trends <- data.table::rbindlist(list(kommune, bydel))
  trends[, N := .N, by = c("KOMMUNE", filedims, "allpanels", "type")]
  trends <- trends[N > 1]

  return(trends)
}
