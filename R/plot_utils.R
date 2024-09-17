#' @title get_plotsavefolder
#' @description
#' identify the correct folder to save plots into
#'
#' @keywords internal
#' @noRd
get_plotsavefolder <- function(cubename,
                               plotfolder = c("Boxplot",
                                              "Boxplot_change",
                                              "TimeSeries",
                                              "TimeSeries_change",
                                              "TimeSeries_bydel",
                                              "TimeSeries_country",
                                              "Diff_timetrends")){
  plotfolder <- match.arg(plotfolder)
  path <- file.path(getOption("qualcontrol.root"),
                    getOption("qualcontrol.output"),
                    getOption("qualcontrol.year"),
                    cubename,
                    "PLOTT",
                    plotfolder)
  if(!dir.exists(path)) generate_qcfolders(cubename, year = getOption("qualcontrol.year"))
  return(path)
}

#' @title get_plot_subset
#' @keywords internal
#' @noRd
#' @description
#' Find the optimal way to divide plots into separate files by dimensions.
#' The algorithm select the combination of dimensions yielding the maximum number of panels per page which is <= maxpanels
#'
#' @param dt dataset
#' @param by all bycols
#' @param maxpanels maximum number of panels per page
get_plot_subset <- function(dt,
                            by,
                            maxpanels = 25){

  orgstrata <- nrow(dt[, .N, by = by])

  if(orgstrata <= maxpanels){
    return(NULL)
  }

  # Make reftable of dims and levels
  ref <- data.table::data.table(dim = by, n = integer(length(by)))
  for(i in by){
    ref[dim == i, let(n = length(unique(dt[[i]])))]
  }

  # make table of all combinations of dimensions
  combs <- data.table::data.table()
  for(i in seq_along(by)){
    x <- data.table::data.table(base::t(utils::combn(by, i)))
    colnames(x) <- paste0("dim", 1:i)
    combs <- data.table::rbindlist(list(combs, x), fill = TRUE)
  }

  # add levels for each dimension
  for(i in seq_along(by)){
    new <- paste0("n", i)
    refdim <- paste0("dim", i)
    combs[ref, (new) := i.n, on = setNames("dim", refdim)]
  }

  # multiply all levels for all combinations to get files
  data.table::setnafill(combs, fill = 1, cols = grep("^n", names(combs)))
  combs[, files := Reduce("*", .SD), .SDcols = data.table::patterns("^n")]

  # Find excluded dimensions per row, multiply levels to get panels per file
  incdims <- combs[, do.call(paste, c(.SD, sep = "|")), .SDcols = patterns("^dim")]
  nondims <- list()
  for(i in seq_along(incdims)){
    nondims[[i]] <- grep(incdims[i], by, invert = T, value = T)
  }
  nondims <- lapply(nondims, function(x) ref[dim %in% x, n])
  nondims <- as.integer(lapply(nondims, function(x) if(length(x) > 0) Reduce("*", x) else 1))
  combs[, panels := nondims]

  optimal <- combs[panels <= maxpanels][panels == max(panels)]
  if(nrow(optimal > 1)){
    optimal <- optimal[1]
  }

  subset <- unlist(optimal[, .SD, .SDcols = patterns("^dim")], use.names = F)
  subset <- subset[!is.na(subset)]
  return(subset)
}

#' @title get_plot_filter
#' @keywords internal
#' @noRd
#' @description
#' [qualcontrol::get_plot_subset()] generate a vector of columns to filter data by when plotting to separate files.
#' This function generates the actual filtering strings to be used.
#' @param dt plotdata
#' @param by Character vector of columns to filter by
get_plot_filter <- function(dt,
                            by = NULL){
  if(length(by) == 0){
    return("TRUE")
  }

  subsets <- collapse::GRP(dt, by)[["groups"]]
  for(i in names(subsets)){
    subsets[, (i) := paste0(i, "=='", get(i), "'")]
  }
  subsets[, filter := do.call(paste, c(.SD, sep = " & "))]

  return(subsets$filter)
}


