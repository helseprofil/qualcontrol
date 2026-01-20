#' @title plot_timeseries
#' @description
#' Generate timeseries plots indicating outliers (values outside q25-1.5IQR, q75+1.5IQR).
#' @param dt flagged data file, default to newcube_flag
#' @param show_n_years Only show x most recent years in the plots, deafalts to 10. To show all, set value to NULL.
#' @param onlynew should only new outliers be plotted? Default = TRUE
#' @param change Should year-to-year changes be plotted? Default = FALSE
#' @param show_change_low minimum decrease you want to plot, default = -10 (%)
#' @param show_change_high minimum increase you want to plot, default = 20 (%)
#' @param save Should plots be saved to the default folder? Default = T
#' @return saved plots
#' @export
  plot_timeseries <- function(dt = newcube_flag,
                              show_n_years = 10,
                              onlynew = TRUE,
                              change = FALSE,
                              show_change_low = -10,
                              show_change_high = 20,
                              save = TRUE){

  if(length(unique(dt$AAR)) < 2){
    cat("Only one unique year in the file, time series not plotted")
    return(invisible(NULL))
  }
  colinfo <- identify_coltypes(dt)
  cubename <- get_cubename(dt)
  cubefile <- get_cubefilename(dt)
  folder <- ifelse(change, "TimeSeries_change", "TimeSeries")
  savepath <- get_plotsavefolder(cubename, folder)
  if(save){
    archive_old_files(savepath, ".png")
    on.exit(try(dev.off(), silent = T), add = TRUE)
  }

  tscols <- get_plot_cols(dt, change = change, colinfo = colinfo, plot = "ts")
  plotvalue <- tscols$plotvalue
  outlier <- tscols$outlier
  newoutlier <- tscols$newoutlier
  teller <- select_teller_pri(names(dt))
  if(plotvalue %notin% names(dt)) stop(plotvalue, " not found in data, plot not generated")
  isnewoutlier <- newoutlier %in% names(dt)
  if(onlynew & !isnewoutlier) onlynew <- FALSE

  keepcols <- intersect(unique(c(colinfo$dims.new, unlist(tscols, use.names = F), teller)), names(dt))
  complete <- dt[, !is.na(GEOniv) & !is.na(x), env = list(x = as.name(plotvalue))]
  d <- dt[complete, .SD, .SDcols = keepcols]
  d[, AARh := sub("\\d{4}_(\\d{4})", "\\1", AAR)]

  if(!is.null(show_n_years)){
    incl_aar <- (max(as.numeric(d$AARh)) - show_n_years - 1):max(as.numeric(d$AARh))
    d <- d[AARh %in% incl_aar]
  }

  bycols <- c("GEO", setdiff(colinfo$dims.new, c("GEO", "AAR")))
  data.table::setkeyv(d, c(bycols, "AARh"))

  outlierfilter <- ifelse(onlynew, newoutlier, outlier)
  g <- collapse::GRP(d, by = bycols)
  n_outlier <- collapse::fsum(d[[outlierfilter]], g = g)
  n_obs <- collapse::fsum(!is.na(d[[plotvalue]]), g = g)
  y_middle <- 0.5*(collapse::fmax(d[[plotvalue]], g = g) + collapse::fmin(d[[plotvalue]], g = g))

  strata <- collapse::fmutate(g[["groups"]],
                              n_outlier = n_outlier,
                              n_obs = n_obs,
                              y_middle = y_middle)
  if(change){
    min_plotvalue <- collapse::fmin(d[[plotvalue]], g = g)
    max_plotvalue <- collapse::fmax(d[[plotvalue]], g = g)
    strata[, let(min_plotvalue = min_plotvalue, max_plotvalue = max_plotvalue)]
    strata <- strata[min_plotvalue < show_change_low | max_plotvalue > show_change_high]
  }

  strata <- strata[n_outlier > 0L & n_obs > 0L]

  if(nrow(strata) == 0){
    cat("No strata with", outlierfilter, "= 1 found in the data, plots not generated.")
    if(!is.null(show_n_years)) cat(" (Only tried to plot", show_n_years, "most recent years)")
    return(invisible(NULL))
  }

  strata[, let(page = ((.I - 1L) %/% 25) + 1L, # max 25 paneler per side
               panels = interaction(.SD, drop = TRUE, sep = ",", lex.order = T)), .SDcols = bycols]
  plotdata <- collapse::join(strata, d, on = bycols, how = "left", multiple = TRUE, verbose = 0, overid = 2)
  plotdata[, let(yval = x, tv = round(y,0), ol = z), env=list(x = plotvalue, y = teller, z = outlier)]
  plotdata[ol == 1, let(ollabel = "New outlier")]
  if(isnewoutlier){
    plotdata[ol == 1, ollabel := data.table::fcase(x == 0, "Previous outlier", default = ollabel), env = list(x = newoutlier)]
  }
  plotdata[ol == 0, ollabel := "Normal"]
  plotdata[, ollabel := factor(ollabel, levels = c("Normal", "Previous outlier", "New outlier"))]

  metadata <- plotdata[, .(geosuffix = paste0(min(GEO), "-", max(GEO))), by = page][, let(filename = paste0(cubefile,"_GEO_", geosuffix))]
  metadata[, let(dup_idx = seq_len(.N)), by = filename]
  metadata[dup_idx > 1, let(filename = paste0(filename, "(", dup_idx, ")"))][, let(tmp_name = sprintf("plot-%04d.png", page),
                                                                                   filename = paste0(filename, ".png"),
                                                                                   dup_idx = NULL)]

  plotdata <- plotdata[, .SD, .SDcols = c("AARh", "yval", "y_middle", "tv", "ol", "ollabel", "n_obs", "panels", "page")]
  plotdata <- split(plotdata, by =  "page")

  n_plot <- metadata[, .N]

  plotargs <- list(teller = teller,
                   title = paste0(attributes(dt)$Filename, ", Plotted: ", Sys.Date()))
  plotargs$caption <- paste0("Tellervariabel: ", teller, "\nPlots grouped by: ", paste0(bycols, collapse = ", "))
  plotargs$ylab <- ifelse(change, paste0(sub("change_", "", plotvalue), ", (% change)"), plotvalue)
  if(onlynew) plotargs$caption <- paste0(plotargs$caption, "\nOnly strata with new outliers. Comparison file: ", attributes(dt)$comparison)

  dpi = 170
  size <- compute_device_size_px(plot_timeseries_plotfun(datasets = collect_timeseries_plotdata(plotdata, 1), plotargs = plotargs),
                                 dpi = dpi)

  pb <- progress::progress_bar$new(format = "Plotter :current / :total filer. [:bar] Estimert ferdig om: :eta",
                                   total = n_plot, clear = FALSE)
  if(save){
    ragg::agg_png(filename = file.path(savepath, "plot-%04d.png"), res = dpi, width = size$width_px, height = size$height_px, units = "px")
  }
  for(i in seq_len(n_plot)){
    plot <- plot_timeseries_plotfun(datasets = collect_timeseries_plotdata(plotdata, page = i), plotargs = plotargs)
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

#' @keywords internal
#' @noRd
compute_device_size_px <- function(p, dpi = 160) {

  g <- ggplot2::ggplotGrob(p)

  total_w_cm <- as.numeric(grid::convertWidth(sum(g$widths), "cm", valueOnly = TRUE))
  total_h_cm <- as.numeric(grid::convertHeight(sum(g$heights), "cm", valueOnly = TRUE))

  width_px  <- ceiling(total_w_cm * dpi / 2.54)
  height_px <- ceiling(total_h_cm * dpi / 2.54)

  list(width_px = width_px, height_px = height_px)
}

#' @keywords internal
#' @noRd
collect_timeseries_plotdata <- function(plotdata, page){
  plot_d <- list()
  plot_d[["base"]] <- plotdata[[page]]
  # plot_d[["ol"]] <- plot_d[["base"]][ol == 1]
  plot_d[["line"]] <- plot_d[["base"]][n_obs > 1]
  return(plot_d)
}

#' @title plot_timeseries_plotfun
#' @description
#' Save function for [qualcontrol::plot_timeseries()]
#' @keywords internal
#' @noRd
plot_timeseries_plotfun <- function(datasets, plotargs){

  plot <- ggplot2::ggplot(datasets$base, ggplot2::aes(x = AARh, y = yval)) +
    ggplot2::facet_wrap(facets = ggplot2::vars(panels), scales = "free_y", ncol = 5) +
    ggplot2::geom_point(ggplot2::aes(color = ollabel), size = 1.5, show.legend = TRUE) +
    ggplot2::scale_color_manual(values = c("Normal" = "grey40", "Previous outlier" = "blue", "New outlier" = "red"),
                                limits = c("Normal", "Previous outlier", "New outlier"),
                                breaks = c("Previous outlier", "New outlier"),
                                drop = FALSE)  +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    ggplot2::geom_line(data = datasets$line, ggplot2::aes(group = panels), linewidth = 0.3, na.rm = T) +
    theme_qc()

  if(!is.na(plotargs$teller)){
    plot <- plot +
      ggplot2::geom_text(ggplot2::aes(label = tv, y = y_middle), hjust = 0.5, angle = 90, size = 9/ggplot2::.pt)
  }

  plot <- plot +
    ggh4x::force_panelsizes(cols = ggplot2::unit(5, "cm"),
                            rows = ggplot2::unit(3.6, "cm")) +
    ggplot2::labs(title = plotargs$title,
                  y = plotargs$ylab,
                  caption = plotargs$caption) +
    ggplot2::theme(text = ggplot2::element_text(family = "sans"),
                   plot.title = ggplot2::element_text(size = 20),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 8),
                   strip.text = ggplot2::element_text(hjust = 0, size = 9))

  return(plot)
}

