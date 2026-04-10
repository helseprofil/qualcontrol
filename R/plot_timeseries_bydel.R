#' @title plot_timeseries_bydel
#' @description
#' Generate timeseries plots for bydel, with weighted
#' @param dt flagged data file, default to newcube_flag
#' @export
plot_timeseries_bydel <- function(dt = newcube_flag, save = TRUE){
  if(nrow(dt[GEOniv == "B"]) == 0){
    cat("No data on bydel, no check performed")
    return(invisible(NULL))
  }
  if(length(unique(dt$AAR)) < 2){
    cat("Only one unique year in the file, time series not plotted")
    return(invisible(NULL))
  }

  d <- data.table::copy(dt)[(GEO %in% c(301, 1103, 4601, 5001) | GEOniv == "B")]
  colinfo <- identify_coltypes(d)
  plotvalue <- select_outlier_pri(d, colinfo = colinfo)
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  savepath <- get_plotsavefolder(cubename, "TimeSeries_bydel")
  if(save) archive_old_files(savepath, ".png")

  contains_bydel <- d[GEOniv == "B" & SPVFLAGG == 0, unique(AAR)]
  d <- d[AAR %in% contains_bydel & !is.na(x), .SD,
         .SDcols = c(colinfo$dims.new, plotvalue, "GEOniv"),
         env = list(x = plotvalue)]
  add_kommune(d)
  bycols <- c("KOMMUNE", grep("^GEO$", colinfo$dims.new, invert = T, value = T))

  panels <- grep("^KOMMUNE$|^AAR$", bycols, invert = T, value = T)
  filedims <- get_plot_subset(d, panels, maxpanels = 5)
  if(length(filedims) > 0) panels <- panels[panels %notin% filedims]
  filter <- get_plot_filter(d, filedims)
  d[, let(allpanels = "alle")]
  subtitlepanels <- character()
  for(dim in panels){
    if(length(unique(dt[[dim]])) == 1){
      subtitlepanels <- c(subtitlepanels, paste0(dim, " = ", unique(dt[[dim]])))
      panels <- setdiff(panels, dim)
    }
  }

  if(length(panels) > 0){
    d[, allpanels := interaction(.SD), .SDcols = panels]
  }

  trends <- plot_timeseries_bydel_trendlines(d, bycols, filedims, plotvalue)

  # Generate global plot elements
  plotargs <- list()
  plotargs$plotvalue <- plotvalue
  plotargs$title <- paste0("File: ", attributes(dt)$Filename, ", Plotting date: ", Sys.Date())
  plotargs$subtitlepanels <- character()
  if(length(subtitlepanels) > 0) plotargs$subtitlepanels <- paste(subtitlepanels, collapse = "\n")
  plotargs$allplotdims <- get_all_combinations(d, c("KOMMUNE", "allpanels"))
  plotargs$anyrows <- ifelse(length(panels) > 0, 1, 0)
  if(plotargs$anyrows == 1){
    plotargs$caption <- paste0("Rader fordelt på ", paste(panels, collapse = ", "))
  }
  rows <- nrow(plotargs$allplotdims[, .N, by = allpanels])

  metadata <- data.table::data.table(file = seq_len(length(filter)), filter = filter)
  suffix <- character()
  for(i in metadata$file) suffix <- c(suffix, get_multifile_plot_suffix(d[x, env = list(x = str2lang(filter[[i]]))], filedims))
  metadata[, let(tmp_name = sprintf("plot-%04d.png", file),
                 filename = paste0(cubefile, "_", suffix, ".png"))]
  n_plot <- metadata[, .N]

  pb <- progress::progress_bar$new(format = "Plotter :current / :total filer. [:bar] Estimert ferdig om: :eta",
                                   total = n_plot, clear = FALSE)

  dpi = 220
  maxrowfile <- 1
  if(length(filter) > 1){
    n_panels <- integer()
    for(i in seq_along(filter)){
      n_panels <- c(n_panels, d[x, env = list(x = str2lang(filter[[i]]))][, length(unique(panels))])
    }
    maxrowfile <- which.max(n_panels)
  }
  size <- compute_device_size_px(plot_timeseries_bydel_plotfun(collect_tsb_plotdata(d, trends, filter, maxrowfile), plotargs), dpi = dpi)

  if(save){
    ragg::agg_png(filename = file.path(savepath, "plot-%04d.png"), res = dpi, width = size$width_px, height = size$height_px, units = "px")
  }

  for(i in metadata$file){
    plotdata <- collect_tsb_plotdata(d, trends, filter, i)
    plotargs$subtitle <- character()
    for(dim in filedims) plotargs$subtitle <- paste0(plotargs$subtitle, paste0(dim, ": ", unique(plotdata$pd[[dim]]), collapse = "\n"))
    if(length(plotargs$subtitlepanels) > 0) plotargs$subtitle <- paste0(plotargs$subtitlepanels, "\n", plotargs$subtitle)
    plot <- plot_timeseries_bydel_plotfun(plotdata, plotargs)
    if(save) print(plot)
    pb$tick()
  }

  if(save){
    dev.off()
    for (k in 1:30) {
      if (all(file.exists(file.path(savepath, metadata$tmp_name)))) break
      Sys.sleep(0.1)
    }
    invisible(file.rename(file.path(savepath, metadata$tmp_name),file.path(savepath, metadata$filename)))
  }
}

#' @title collect_tsb_plotdata
#' @keywords internal
#' @noRd
collect_tsb_plotdata <- function(plotdata, trenddata, filter, file){
  data <- list()
  data[["pd"]] <- plotdata[x, env = list(x = str2lang(filter[[file]]))]
  data[["td"]] <- trenddata[x, env = list(x = str2lang(filter[[file]]))]
  return(data)
}

#' @title plot_boxplot_plotfun
#' @description
#' Plotting function for [qualcontrol::plot_timeseries_bydel()]
#' @keywords internal
#' @noRd
#' @param pd plotdata
#' @param td trenddata
#' @param plotargs list of plot arguments
plot_timeseries_bydel_plotfun <- function(plotdata, plotargs){

  plot <- ggplot2::ggplot(plotargs$allplotdims) +
    ggplot2::facet_grid(cols = ggplot2::vars(KOMMUNE),
                        rows = ggplot2::vars(allpanels),
                        switch = "y",
                        scales = "free_y") +
    ggplot2::labs(title = plotargs$title,
                  subtitle = plotargs$subtitle,
                  y = plotargs$plotvalue) +
    ggplot2::geom_line(data = plotdata$td,
                       ggplot2::aes(x = AAR, y = y, color = type, group = type),
                       linewidth = 1.5) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::geom_line(data = plotdata$pd,
                       ggplot2::aes(x = AAR, y = .data[[plotargs$plotvalue]], group = GEO), linetype = 2) +
    ggplot2::geom_point(data = plotdata$pd,
                        ggplot2::aes(x = AAR, y = .data[[plotargs$plotvalue]]),
               size = 3, shape = 1) +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    theme_qc() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   plot.title = ggplot2::element_text(size = 12, family = "sans", hjust = 1)) +
    ggh4x::force_panelsizes(rows = ggplot2::unit(4.5, "cm"),
                            cols = ggplot2::unit(7, "cm"))

  if(plotargs$anyrows == 0){
    plot <- plot +
      ggplot2::theme(strip.background.y = ggplot2::element_blank(),
                     strip.text.y = ggplot2::element_blank())
  } else {
    plot <- plot +
      ggplot2::labs(caption = plotargs$caption)
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
