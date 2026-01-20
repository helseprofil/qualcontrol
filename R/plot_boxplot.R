#' @title plot_boxplot
#' @description
#' Generate boxplots indicating outliers (values outside q25-1.5IQR, q75+1.5IQR).
#' @param dt flagged data file, default to newcube_flag
#' @param onlynew should only new outliers be plotted? Default = TRUE
#' @param change Should year-to-year changes be plotted? Default = FALSE
#' @param save Should plots be saved to the default folder? Default = T
#' @export
plot_boxplot <- function(dt = newcube_flag, onlynew = TRUE, change = FALSE, save = TRUE){

  d <- data.table::copy(dt)[!is.na(GEOniv)]
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  colinfo <- identify_coltypes(d)
  folder <- ifelse(change, "Boxplot_change", "Boxplot")
  savepath <- get_plotsavefolder(cubename, folder)
  if(save) archive_old_files(savepath, ".png")

  bpcols <- get_plot_cols(d, change = change, colinfo = colinfo, plot = "bp")
    plotvalue <- bpcols$plotvalue
    outlier <- bpcols$outlier
    newoutlier <- bpcols$newoutlier
    quantiles <- bpcols$quantiles
    limits <- bpcols$limits
  if(plotvalue %notin% names(d)) stop(plotvalue, " not found in data, plot not generated")
  if(onlynew & bpcols$newoutlier %notin% names(d)) onlynew <- FALSE

  # Extract baseplotdata
  bycols <- c("GEOniv", grep("^GEO$|^AAR$", colinfo$dims.new, invert = T, value = T))
  g <- collapse::GRP(d, c(bycols, quantiles, limits))

  bpdata <- collapse::join(g[["groups"]],
                                 d[, .(N_obs = collapse::fsum(!is.na(get(plotvalue))),
                                       MINABOVELOW = collapse::fmin(get(plotvalue)[get(plotvalue) >= get(limits[1])]),
                                       MAXBELOWHIGH = collapse::fmax(get(plotvalue)[get(plotvalue) <= get(limits[2])])),
                                   by = bycols],
                                 verbose = 0, overid = 2)
  bpdata[, (limits) := NULL]

  panels <- grep("^GEOniv$", bycols, invert = T, value = T)
  filedims <- get_plot_subset(bpdata, panels, maxpanels = 25)
  if(length(filedims > 0)) panels <- panels[panels %notin% filedims]
  filter <- get_plot_filter(bpdata, filedims)
  plotby <- c("GEOniv", panels)

  allcombinations <- get_all_combinations(bpdata, plotby)
  allcombinations[,  panels := interaction(.SD, drop = TRUE, lex.order = T, sep = ","), .SDcols = panels]
  bpdata <- collapse::join(allcombinations, bpdata, multiple = T, overid = 2, verbose = 0)[, names(.SD) := NULL, .SDcols = panels]
  bpdata[, GEOniv := droplevels(GEOniv)]
  bpdata[, GEOniv := factor(GEOniv, levels = rev(levels(GEOniv)))]

  if(onlynew){
    oldata <- d[x == 1, env = list(x = newoutlier)]
  } else {
    oldata <- d[x == 1, env = list(x = outlier)]
  }
  oldata[, let(label = paste0(GEO, "'", sub(".*(\\d{2}$)", "\\1", AAR),"'(", round(x, 0), ")"),
               yval = x), env = list(x = plotvalue)]
  oldata <- oldata[, .SD, .SDcols = c(bycols, "label", "yval")]
  oldata <- collapse::join(allcombinations, oldata, on = plotby, multiple = T, overid = 2, verbose = 0)[, names(.SD) := NULL, .SDcols = panels]

  plotargs <- list(quantiles = quantiles)
  plotargs[["title"]] <- paste0("File: ", attributes(dt)$Filename, ", Plotting date: ", Sys.Date())
  plotargs[["caption"]] <- paste0("Plots grouped by: ", paste0(panels, collapse = ", "))
  if(onlynew) plotargs$caption <- paste0(plotargs$caption, "\nOnly new outliers indicated. Comparison file: ", attributes(dt)$comparison)
  plotargs[["ylab"]] <- ifelse(change, paste0(sub("change_", "", plotvalue), ", (% change)"), plotvalue)

  dpi = 220
  size <- compute_device_size_px(plot_boxplot_plotfun(collect_boxplot_plotdata(bpdata, oldata, filter, 1), plotargs), dpi = dpi)

  metadata <- data.table::data.table(file = seq_len(length(filter)), filter = filter)
  suffix <- character()
  for(i in metadata$file) suffix <- c(suffix, get_multifile_plot_suffix(bpdata[x, env = list(x = str2lang(filter[[i]]))], filedims))
  metadata[, let(tmp_name = sprintf("plot-%04d.png", file),
                 filename = paste0(cubefile, "_", suffix, ".png"))]
  n_plot <- metadata[, .N]

  pb <- progress::progress_bar$new(format = "Plotter :current / :total filer. [:bar] Estimert ferdig om: :eta",
                                   total = n_plot, clear = FALSE)


  if(save){
    ragg::agg_png(filename = file.path(savepath, "plot-%04d.png"), res = dpi, width = size$width_px, height = size$height_px, units = "px")
  }

  for(i in metadata$file){
    plotdata = collect_boxplot_plotdata(bpdata, oldata, filter, i)
    plotargs[["subtitle"]] <- character()
    for(dim in filedims) plotargs$subtitle <- c(plotargs$subtitle, paste0("\n", dim, ": ", unique(plotdata$bp[[dim]])))
    plot <- plot_boxplot_plotfun(plotdata, plotargs = plotargs)
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

collect_boxplot_plotdata <- function(bpdata, oldata, filter, file){
  data <- list()
  data[["bp"]] <- bpdata[x, env = list(x = str2lang(filter[[file]]))][N_obs > 2]
  data[["ol"]] <- oldata[x, env = list(x = str2lang(filter[[file]]))]
  return(data)
}

#' @title plot_boxplot_plotfun
#' @description
#' Plotting function for [qualcontrol::plot_boxplot()]
#' @keywords internal
#' @noRd
#' @param bp data for generating the boxplots
#' @param ol data with outliers
#' @param plotargs list of plot arguments
plot_boxplot_plotfun <- function(plotdata, plotargs){

  plot <- ggplot2::ggplot(data = plotdata$bp,
                          ggplot2::aes(x = GEOniv)) +
    ggplot2::facet_wrap(ggplot2::vars(panels),
                        scales = "free_x",
                        ncol = 5) +
    ggplot2::labs(y = plotargs$ylab,
                  x = NULL,
                  title = plotargs$title,
                  subtitle = plotargs$subtitle,
                  caption = plotargs$caption) +
    ggplot2::coord_flip() +
    ggplot2::geom_boxplot(data = plotdata$bp,
                          ggplot2::aes(ymin = MINABOVELOW,
                                       lower = get(plotargs$quantiles[1]),
                                       middle = get(plotargs$quantiles[2]),
                                       upper = get(plotargs$quantiles[3]),
                                       ymax = MAXBELOWHIGH),
                          stat = "identity") +
    ggplot2::geom_text(data = plotdata$ol,
                       ggplot2::aes(y = yval, label = label),
                       na.rm = T,
                       angle = 90,
                       size = 6/ggplot2::.pt) +
    ggh4x::force_panelsizes(cols = ggplot2::unit(8, "cm"),
                            rows = ggplot2::unit(6, "cm")) +
    theme_qc() +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 12),
                   plot.caption = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 8),
                   strip.text = ggplot2::element_text(hjust = 0, size = 9))

  return(plot)
}

#' @title plot_boxplot_savefun
#' @description
#' Save function for [qualcontrol::plot_boxplot()]
#' @keywords internal
#' @noRd
plot_boxplot_savefun <- function(plot,
                                 savepath,
                                 cubefile,
                                 suffix,
                                 n_rows){

  savename <- paste0(cubefile, "_", suffix, ".png")
  height = n_rows*6 + 12

  ggplot2::ggsave(file.path(savepath, savename),
                  plot,
                  width = 45,
                  height = height,
                  units = "cm")
}
