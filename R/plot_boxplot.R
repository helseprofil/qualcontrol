#' @title plot_boxplot
#' @description
#' Generate boxplots indicating outliers (values outside q25-1.5IQR, q75+1.5IQR).
#' @param dt flagged data file, default to newcube_flag
#' @param onlynew should only new outliers be plotted? Default = TRUE
#' @param change Should year-to-year changes be plotted? Default = FALSE
#' @param save Should plots be saved to the default folder? Default = T
#' @export
plot_boxplot <- function(dt = newcube_flag,
                         onlynew = TRUE,
                         change = FALSE,
                         save = TRUE){

  d <- data.table::copy(dt)[!is.na(GEOniv)]
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  colinfo <- identify_coltypes(d)
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

  baseplotdata <- collapse::join(g[["groups"]],
                                 d[, .(N_obs = collapse::fsum(!is.na(get(plotvalue))),
                                       MINABOVELOW = collapse::fmin(get(plotvalue)[get(plotvalue) >= get(limits[1])]),
                                       MAXBELOWHIGH = collapse::fmax(get(plotvalue)[get(plotvalue) <= get(limits[2])])),
                                   by = bycols],
                                 verbose = 0, overid = 0)
  baseplotdata[, (limits) := NULL]

  # Extract outlierdata
  if(onlynew) outlierdata <- d[get(newoutlier) == 1]
  if(!onlynew) outlierdata <- d[get(outlier) == 1]
  outlierdata[, label := paste0(GEO, "'", sub(".*(\\d{2}$)", "\\1", AAR))]
  outlierdata <- outlierdata[, c(..bycols, ..plotvalue, "label")]

  # Generate filter for multifile plotting
  panels <- grep("^GEOniv$", bycols, invert = T, value = T)
  filedims <- get_plot_subset(baseplotdata, panels, maxpanels = 25)
  if(length(filedims > 0)) panels <- panels[panels %notin% filedims]
  filter <- get_plot_filter(baseplotdata, filedims)

  plotby <- c("GEOniv", panels)

  # Create general plot parameters
  plotargs <- list(plotvalue = plotvalue,
                   panels = panels,
                   quantiles = quantiles)
  plotargs$allplotdims <- get_all_combinations(baseplotdata, plotby)
  plotargs$title <- paste0("File: ", attributes(dt)$Filename, ", Plotting date: ", Sys.Date())
  plotargs$caption <- paste0("Plots grouped by: ", paste0(panels, collapse = ", "))
  plotargs$ylab <- ifelse(change, paste0(sub("change_", "", plotvalue), ", (% change)"), plotvalue)
  plotargs$subtitle <- paste0("Variable plotted: ", plotargs$ylab)
  if(onlynew) plotargs$subtitle <- paste0(plotargs$subtitle, ", only new outliers indicated. Comparison file: ", attributes(dt)$comparison)

  n_rows <- ceiling(nrow(plotargs$allplotdims[, .N, by = panels])/5)
  folder <- ifelse(change, "Boxplot_change", "Boxplot")
  savepath <- get_plotsavefolder(cubename, folder)
  if(save) archive_old_files(savepath, cubename)

  for(i in filter){
    if(save) cat("\nSaving file", which(filter == i), "/", length(filter))
    bp <- baseplotdata[eval(parse(text = i))][N_obs > 2]
    ol <- outlierdata[eval(parse(text = i))]

    if(nrow(bp) > 0){
      suffix <- get_multifile_plot_suffix(bp, filedims)
      plotargs$subtitle_full <- plotargs$subtitle
        for(i in filedims){
          plotargs$subtitle_full <- paste0(plotargs$subtitle_full, "\n", i, ": ", unique(bp[[i]]))
        }
      plot <- plot_boxplot_plotfun(bp, ol, plotargs)
      if(save) plot_boxplot_savefun(plot, savepath, cubefile, suffix, n_rows)
      print(plot)
    }
  }
}

#' @title plot_boxplot_plotfun
#' @description
#' Plotting function for [qualcontrol::plot_boxplot()]
#' @keywords internal
#' @noRd
#' @param bp data for generating the boxplots
#' @param ol data with outliers
#' @param plotargs list of plot arguments
plot_boxplot_plotfun <- function(bp,
                                 ol,
                                 plotargs){

  plot <- ggplot2::ggplot(data = plotargs$allplotdims,
                          ggplot2::aes(x = GEOniv)) +
    ggplot2::facet_wrap(plotargs$panels,
                        labeller = ggplot2::labeller(.multi_line = F),
                        scales = "free_x",
                        ncol = 5) +
    ggplot2:: scale_x_discrete(limits = rev(levels(ol$GEOniv)),
                               drop = T) +
    ggplot2::labs(y = plotargs$ylab,
                  x = NULL,
                  title = plotargs$title,
                  subtitle = plotargs$subtitle_full,
                  caption = plotargs$caption) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(data = ol,
                       ggplot2::aes(y = get(plotargs$plotvalue),
                                    label = label),
                       angle = 90,
                       size = 8/ggplot2::.pt) +
    ggplot2::geom_boxplot(data = bp,
                          ggplot2::aes(ymin = MINABOVELOW,
                                       lower = get(plotargs$quantiles[1]),
                                       middle = get(plotargs$quantiles[2]),
                                       upper = get(plotargs$quantiles[3]),
                                       ymax = MAXBELOWHIGH),
                          stat = "identity") +
    ggh4x::force_panelsizes(cols = ggplot2::unit(8, "cm"),
                            rows = ggplot2::unit(6, "cm")) +
    theme_qc() +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 16),
                   plot.caption = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_text(size = 16),
                   axis.text = ggplot2::element_text(size = 12))

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
