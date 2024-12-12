#' @title comparecube_summary
#' @description
#' Generate a table estimating, for each strata by GEOniv/value column,
#' the number of identical/different rows, new/expired censored observations,
#' and mean/min/max diff / reldiff.
#'
#' Uses [qualcontrol::qc_round()] on the data to eliminate very marginal differences.
#' @param dt comparecube
#'
#' @return a DT output table
#' @export
comparecube_summary <- function(dt = comparecube){

  if(is.null(dt)) return(invisible(NULL))
  dt <- data.table::copy(dt[newrow == 0]) |> qc_round() |> translate_geoniv()
  diffvals <- gsub("_diff", "", grep("_diff$", names(dt), value = T))
  tabvals <- c("Identical", "Different", "New_prikk", "Expired_prikk",
               "Mean_diff", "Min_diff", "Max_diff",
               "Mean_reldiff", "Min_reldiff", "Max_reldiff")
  data.table::setkeyv(dt, c("GEOniv", data.table::key(dt)))
  geolevels <- c("TOTAL", levels(dt$GEOniv))

  out <- data.table::CJ(GEOniv = forcats::fct_inorder(geolevels),
                        Value = forcats::fct_inorder(diffvals))
  out[, (tabvals) := NA_real_]

  for(geolevel in geolevels){
    subset <- dt
    if(geolevel != "TOTAL"){ subset <- subset[GEOniv == geolevel] }
    summarise_diffvals(out, subset, diffvals, geolevel)
  }

  nosearch <- grep("^GEOniv$|^Value$", names(out), invert = T, value = T)
  return(tab_output(out,
                    nosearchcolumns = nosearch))
}

#' @title diffvals_summary
#' @description
#' Summarises number of diffrows and the sum of the diffs.
#'
#' @param dt comparecube
#' @param byyear Get output by year, default = FALSE
#' @export
diffvals_summary <- function(dt = comparecube,
                             byyear = FALSE){

  d <- data.table::copy(dt[newrow == 0])
  diffvals <- grep("_diff$", names(d), value = T)
  diffvals_rmdiff <- sub("_diff", "", diffvals)
  diffvals_rmTN <- grep("TELLER|NEVNER", diffvals_rmdiff, invert = T, value = T)
  teller <- select_teller_pri(diffvals_rmdiff)
  nevner <- select_nevner_pri(diffvals_rmdiff)
  diffvals <- c(teller, nevner, diffvals_rmTN)

  if(!byyear){
    out <- list(AAR = "TOTAL")
    for(val in diffvals){
      diff <- paste0(val, "_diff")
      out[[paste0(val, "_ndiff")]] <- d[get(diff) != 0, .N]
      out[[paste0(val, "_sumdiff")]] <- d[, sum(get(diff), na.rm = T)]
    }
    data.table::setDT(out)
    return(tab_output(qc_round(out), dom = "t", filter = "none"))
  }

  out <- data.table::data.table(AAR = unique(d$AAR))
  for(val in diffvals){
    diff <- paste0(val, "_diff")
    dd <- d[, .(ndiff = sum(get(diff) != 0, na.rm =T),
              sumdiff = sum(get(diff), na.rm = T)),
          by = AAR]
    data.table::setnames(dd,
                         old = 2:3,
                         new = function(x) paste0(val, "_", x))
    out <- collapse::join(out, dd, how = "left", on = "AAR", verbose = 0, overid = 0)
  }
  return(tab_output(qc_round(out), dom = "tp", filter = "none"))
}

#' @title plot_diff_timeseries
#' @description
#' Plots differences between new and old cube file across time.
#' Used to see if there are any patterns in the diffs over time. For indicators with strong trends,
#' the differences are expected to become smaller with time as the data is standardized to the last year/period
#' @param dt comparecube
#' @param save Should plots be saved in the Diff_timetrends folder? Default = TRUE. If FALSE, the plot is just printed in the console.
#' @return plot
#' @export
plot_diff_timetrends <- function(dt = comparecube,
                                 save = TRUE){
  if(is.null(dt)) return(invisible(NULL))

  diffval <- select_diffval_pri(names(dt))
  if(is.na(diffval)) return(invisible(NULL))
  reldiffval <- sub("_diff", "_reldiff", diffval)
  labelval <- sub("_diff", "", diffval)
  cubename <- get_cubename(dt)
  cubefile <- get_cubefilename(dt)

  d <- data.table::copy(dt[newrow == 0 & SPVFLAGG_new == 0 & SPVFLAGG_old == 0]) |>
    translate_geoniv()

  d[, let(Absolute = get(diffval),
          Relative = get(reldiffval))]
  d <- data.table::melt(d, measure.vars = c("Absolute", "Relative"))[, .(GEOniv, AAR, variable, value)]
  allyears <- get_all_combinations(d, c("GEOniv", "AAR", "variable"))
  d <- d[!(variable == "Absolute" & value == 0 | variable == "Relative" & value == 1)]
  if(nrow(d) == 0) return("No rows with diffs, no plots generated")

  plotdata <- collapse::join(allyears, d, on = c("GEOniv", "AAR", "variable"), how = "left", multiple = T, verbose = 0, overid = 0)
  xsize <- ifelse(length(unique(plotdata$AAR)) > 12, 10, 20)
  savepath <- get_plotsavefolder(cubename, "Diff_timetrends")
  if(save) archive_old_files(savepath, cubename)

  for(geoniv in unique(d$GEOniv)){
    subset <- plotdata[GEOniv == geoniv]
    plot <- plot_diff_timetrends_plotfun(subset, geoniv, labelval, xsize)
    plot_diff_timetrends_savefun(plot, savepath, cubefile, geoniv, save = save)
    print(plot)
  }
}



## ---- HELPER FUNCTIONS ----

#' @title summarise_diffvals
#' @keywords internal
#' @noRd
#' @description
#' Helper function for [qualcontrol::compare_diffrows()].
#' For each strata by GEOniv/value column, calculates number of identical/different rows,
#' new/expired censored observations, and mean/min/max diff / reldiff
#' Calculates diffs by reference, no need to reassign.
summarise_diffvals <- function(out,
                               subset,
                               diffvals,
                               geolevel){

  for(value in diffvals){
    new <- paste0(value, "_new")
    old <- paste0(value, "_old")
    diff <- paste0(value, "_diff")
    reldiff <- paste0(value, "_reldiff")
    calculate_reldiff <- reldiff %in% names(subset)

    identical <- subset[get(diff) == 0, .N]
    different <- subset[get(diff) != 0, .N]
    newprikk <- subset[is.na(get(new)) & !is.na(get(old)), .N]
    expprikk <- subset[!is.na(get(new)) & is.na(get(old)), .N]

    out[GEOniv == geolevel & Value == value, let(Identical = identical,
                                              Different = different,
                                              New_prikk = newprikk,
                                              Expired_prikk = expprikk)]

    if(different > 0){
      diffdata <- subset[get(diff) != 0 & !is.na(get(new)) & !is.na(get(old))]
      out[GEOniv == geolevel & Value == value, let(Mean_diff = round(mean(diffdata[[diff]], na.rm = T), 3),
                                                   Min_diff = round(min(diffdata[[diff]], na.rm = T), 3),
                                                   Max_diff = round(max(diffdata[[diff]], na.rm = T), 3))]
    }

    if(different > 0 & calculate_reldiff){
      out[GEOniv == geolevel & Value == value, let(Mean_reldiff = round(mean(diffdata[[reldiff]], na.rm = T), 3),
                                                   Min_reldiff = round(min(diffdata[[reldiff]], na.rm = T), 3),
                                                   Max_reldiff = round(max(diffdata[[reldiff]], na.rm = T), 3))]
    }
  }
}

#' @title select_diffval_pri
#' @keywords internal
#' @description
#' Selects first available from MEIS_diff > RATE_diff > SMR_diff
#' @noRd
#' @export
select_diffval_pri <- function(valuecolumns){

  diffval <- data.table::fcase("MEIS_diff"  %in% valuecolumns, "MEIS_diff",
                               "RATE_diff" %in% valuecolumns, "RATE_diff",
                               "SMR_diff" %in% valuecolumns, "SMR_diff",
                               default = NA_character_)
  return(diffval)
}

#' @title plot_diff_timetrends_plotfun
#' @description
#' Plotting function for [plot_diff_timetrends()]
#' @keywords internal
#' @noRd
plot_diff_timetrends_plotfun <- function(plotdata,
                                         geoniv,
                                         label,
                                         xsize){

  plot <- ggplot2::ggplot(plotdata,
                          mapping = ggplot2::aes(x = AAR)) +
    ggplot2::geom_boxplot(ggplot2::aes(x = AAR,
                                       y = value),
                          na.rm = T) +
    ggplot2::facet_wrap(ggplot2::vars(variable),
                        scales = "free_y") +
    ggplot2::labs(title = geoniv,
                  x = "",
                  y = label) +
    theme_qc() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = xsize))

  return(plot)
}

#' @title plot_diff_timetrends_savefun
#' @description
#' Save function for [plot_diff_timetrends()]
#' @keywords internal
#' @noRd
plot_diff_timetrends_savefun <- function(plot,
                                         savepath,
                                         cubefile,
                                         geoniv,
                                         save = TRUE){

  savename <- paste0(cubefile, "_difftrend_", geoniv, ".png")

  if(save){
    ggplot2::ggsave(file.path(savepath, savename),
                    plot,
                    width = ggplot2::unit(12, "cm"),
                    height = ggplot2::unit(7, "cm"))
  }
}
