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

#' @title get_plot_cols
#' @keywords internal
#' @noRd
#' @description
#' Returns a list of relevant columns to contruct boxplots and timeseries plots
#' @param dt plotdata
#' @param change TRUE/FALSE. should the columns be the change variables?
#' @param colinfo object created with [qualcontrol::identify_coltypes()]
#' @param plot one of "bp" or "ts"
#'
#' @return list
get_plot_cols <- function(dt, change, colinfo, plot = c("bp", "ts")){

  cols <- list()

  cols$plotvalue <- attributes(dt)$outlier
  if(is.null(cols$plotvalue)) cols$plotvalue <- select_outlier_pri(dt, colinfo = colinfo)
  cols$outlier <- "OUTLIER"
  cols$newoutlier <- "NEW_OUTLIER"
  if(plot == "bp"){
    cols$quantiles <- c("wq25", "wq50", "wq75")
    cols$limits <- c("LOW", "HIGH")
  }

  if(change) cols <- lapply(cols, function(x) paste0("change_", x))

  return(cols)
}

#' @title get_multifile_plot_suffix
#' @description
#' Gets file name suffix when plotting to separate files.
#'
#' @keywords internal
#' @noRd
get_multifile_plot_suffix <- function(dt, files = files){

  if(length(files) == 0) return("alle")

  suffix <- character()
  for(i in files){
    suffix <- ifelse(length(suffix) == 0,
                     unique(dt[[i]]),
                     paste0(suffix, ",", unique(dt[[i]])))
  }
  return(suffix)
}

# archive_old_plots <- function(){
# First -> Move savepath out of savefunctions, and pass as argument.
# use this function to move all files in folder to "arkiv" before saving new plots

# OR
# Use inside savefun to move one file at a time when identical file exists.
# }
