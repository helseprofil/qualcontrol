#' @title plot_timeseries
#' @description
#' Generate timeseries plots indicating outliers (values outside q25-1.5IQR, q75+1.5IQR).
#' @param dt flagged data file, default to newcube_flag
#' @param onlynew should only new outliers be plotted? Default = TRUE
#' @param change Should year-to-year changes be plotted? Default = FALSE
#' @param save Should plots be saved to the default folder? Default = T
#' @return saved plots
#' @export
plot_timeseries <- function(dt = newcube_flag,
                            onlynew = TRUE,
                            change = FALSE,
                            save = TRUE){

  if(length(unique(dt$AAR)) < 2){
    cat("Only one unique year in the file, time series not plotted")
    return(invisible(NULL))
  }

  d <- data.table::copy(dt)[!is.na(GEOniv)]
  colinfo <- identify_coltypes(d)
  cubename <- get_cubename(d)
  cubefile <- get_cubefilename(d)
  tscols <- get_plot_cols(d, change = change, colinfo = colinfo, plot = "ts")
    plotvalue <- tscols$plotvalue
    outlier <- tscols$outlier
    newoutlier <- tscols$newoutlier
    teller <- select_teller_pri(names(d))
  if(plotvalue %notin% names(d)) stop(plotvalue, " not found in data, plot not generated")
  isnewoutlier <- newoutlier %in% names(d)
  if(onlynew & !isnewoutlier) onlynew <- FALSE

  # Only keep strata with > 1 (new)outlier
  bycols <- grep("^AAR$", colinfo$dims.new, invert = T, value = T)
  outlierfilter <- ifelse(onlynew, newoutlier, outlier)
  d[, let(n_outlier = sum(get(outlierfilter), na.rm = T),
          n_obs = sum(!is.na(get(plotvalue))),
          teller_plot = get(teller)),
    by = bycols]
  d <- d[!is.na(get(plotvalue)) & n_outlier > 0]
  d[, let(y_middle = 0.5*(max(get(plotvalue), na.rm = T) + min(get(plotvalue), na.rm = T))),
    by = bycols]

  if(nrow(d) == 0){
    cat("No strata with", outlierfilter, "= 1, plots not generated")
    return(invisible(NULL))
  }

  # Split into multiple files with max 25 panels per file
  pageinfo <- plot_timeseries_filesplit(d, bycols)
  d <- collapse::join(d, pageinfo, on = bycols, how = "left", multiple = T, overid = 0, verbose = 0)
  outlierdata <- d[get(outlier) == 1]
  if(onlynew & isnewoutlier){
    outlierdata[, label := factor(data.table::fcase(get(newoutlier) == 0, "Previous outlier",
                                                    get(newoutlier) == 1, "New outlier"),
                                  levels = c("Previous outlier", "New outlier"))]
  }
  linedata <- d[n_obs > 1]

  # Create general plot parameters
  n_pages <- max(d$page)
  title <- paste0("File: ", attributes(dt)$Filename, ", Plotting date: ", Sys.Date())
  caption <- paste0("Tellervariabel: ", teller, "\nPlots grouped by: ", paste0(bycols, collapse = ", "))
  ylab <- ifelse(change, paste0(sub("change_", "", plotvalue), ", (% change)"), plotvalue)
  subtitle <- paste0("Variable plotted: ", ylab)
  if(onlynew){
    subtitle <- paste0(subtitle,
                       "\nOnly strata with new outliers. Comparison file: ",
                       attributes(dt)$comparison)
  }

  plotargs <- list(plotvalue = plotvalue,
                   outlier = outlier,
                   newoutlier = newoutlier,
                   teller = teller,
                   isnewoutlier = isnewoutlier,
                   bycols = bycols,
                   title = title,
                   subtitle = subtitle,
                   caption = caption,
                   ylab = ylab)

  for(i in 1:n_pages){
    cat("\nSaving file", i, "/", n_pages)
    d_plot <- d[page == i]
    d_outlier <- outlierdata[page == i]
    d_line <- linedata[page == i]
    if(nrow(d_plot) > 0){
      geosuffix <- paste0(min(d_plot$GEO), "-", max(d_plot$GEO))
      plot <- plot_timeseries_plotfun(d_plot, d_outlier, d_line, plotargs)
      if(save) plot_timeseries_savefun(plot, change, cubename, cubefile, geosuffix)
    }
  }
}

#' @title plot_timeseries_plotfun
#' @description
#' Save function for [qualcontrol::plot_timeseries()]
#' @keywords internal
#' @noRd
plot_timeseries_plotfun <- function(d_plot,
                                    d_outlier,
                                    d_line,
                                    plotargs){

  plot <- ggplot2::ggplot(d_plot,
                          ggplot2::aes(x = AAR, y = get(plotargs$plotvalue))) +
    ggplot2::facet_wrap(facets = plotargs$bycols,
                        scales = "free_y",
                        ncol = 5,
                        labeller = ggplot2::labeller(.multi_line = F)) +
    theme_qc()

  # add outliers
  if(plotargs$isnewoutlier){
    plot <- plot +
      ggplot2::geom_point(data = d_outlier,
                          ggplot2::aes(color = label), size = 3) +
      ggplot2::scale_color_manual(values = c("Previous outlier" = "blue", "New outlier" = "red")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_point()
  } else {
    plot <- plot +
      ggplot2::geom_point(data = d_outlier, color = "red", size = 3) +
      ggplot2::geom_point()
  }

  # add line
  if(nrow(d_line) > 0){
    plot <- plot +
      ggplot2::geom_line(data = d_line,
                         ggplot2::aes(y = get(plotargs$plotvalue), group = 1))
  }

  # add teller
  if(!is.na(plotargs$teller)){
    plot <- plot +
      ggtext::geom_richtext(ggplot2::aes(label = round(get(plotargs$teller),0), y = y_middle),
                            hjust = 0.5, angle = 90, alpha = 0.8, size = 8/ggplot2::.pt)
  }

  plot <- plot +
    ggh4x::force_panelsizes(cols = ggplot2::unit(8, "cm"),
                            rows = ggplot2::unit(5, "cm")) +
    ggplot2::labs(title = plotargs$title,
                  y = plotargs$ylab,
                  caption = plotargs$caption,
                  subtitle = paste0(plotargs$subtitle, "\nGEO codes: ", plotargs$geosuffix)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))


  return(plot)
}

#' @title plot_timeseries_savefun
#' @description
#' Save function for [qualcontrol::plot_timeseries()]
#' @keywords internal
#' @noRd
plot_timeseries_savefun <- function(plot,
                                    change,
                                    cubename,
                                    cubefile,
                                    geosuffix){

  folder <- ifelse(change, "TimeSeries_change", "TimeSeries")
  savepath <- get_plotsavefolder(cubename, folder)
  savename <- paste0(cubefile, "_GEO_", geosuffix, ".png")

  ggplot2::ggsave(file.path(savepath, savename),
                  plot,
                  width = 50,
                  height = 42,
                  units = "cm")
}

# ---- HELPER FUNCTIONS ----

#' @keywords internal
#' @noRd
plot_timeseries_filesplit <- function(dt,
                                      bycols,
                                      maxpanels = 25){

  strata <- collapse::GRP(dt, bycols)
  n_strata <- strata[["N.groups"]]
  pages <- rep(1:ceiling(n_strata/maxpanels), each = maxpanels)[1:n_strata]

  out <- strata[["groups"]]
  out[, let(strata = 1:n_strata,
            page = pages)]
  return(out)
}
