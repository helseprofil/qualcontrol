#' @title make_comparecube
#'
#' @param cube.new
#' @param cube.old
#' @param outliers
#' @param dumps
#'
#' @return
#' @export
#'
#' @examples
make_comparecube <- function(cube.new = NULL,
                             cube.old = NULL,
                             outliers = TRUE,
                             dumps = NULL){

  if(is.null(dumps)) dumps <- getOption("qualcontrol.dumps")
  newcube_flag <- oldcube_flag <- outlierval <- NULL
  colinfo <- identify_coltypes(cube.new, cube.old)
  if(outliers) outlierval <- select_outlier_pri(cube.new, cube.old, colinfo)

  newcube_flag <- flag_rows(cube.new, cube.old, colinfo, "newrow")
  if(outliers) newcube_flag <- flag_outliers(newcube_flag, outlierval)

  if(!is.null(cube.old)){
    oldcube_flag <- flag_rows(cube.new, cube.old, colinfo, "exprow")
    if(outliers) oldcube_flag <- flag_outliers(oldcube_flag, outlierval)
  }

  newcube_flag <<- newcube_flag
  oldcube_flag <<- oldcube_flag
}




#' @title flag_rows
#' @description
#' Flags the new data file for new rows (not in the old file), and the old data file for expired rows (not in the new file).
#' @param cube.new new data file
#' @param cube.old old data file
#' @param colinfo list generated with [qualcontrol::identify_coltypes(cube.new, cube.old)]
#' @param flag one of "newrow" or "exprow", corresponding to which type of column should be flagged.
#'
#' @return flagged data file
#' @export
#' @examples
#' # flag_rows(cube.new, cube.old, colinfo, "newrow")
#' # flag_rows(cube.new, cube.old, colinfo, "exprow")
flag_rows <- function(cube.new,
                      cube.old = NULL,
                      colinfo,
                      flag = c("newrow", "exprow")){

  flag <- match.arg(flag)
  if(flag == "newrow" & is.null(cube.new)) stop("cube.new cannot be NULL when flag = 'newrow'")
  if(flag == "newrow" & is.null(cube.old)) return(data.table::copy(cube.new)[, let(newrow = 1L)])
  if(flag == "exprow" & any(is.null(cube.new), is.null(cube.old))) stop("cube.new and cube.old cannot be NULL when flag = 'exprow'")

  dt <- switch(flag,
               newrow = data.table::copy(cube.new),
               exprow = data.table::copy(cube.old))
  ref <- switch(flag,
                newrow = data.table::copy(cube.old),
                exprow = data.table::copy(cube.new))
  new <- switch(flag,
                newrow = colinfo$newdims,
                exprow = colinfo$expdims)

  dt[, (flag) := 0L]

  for(dim in colinfo$commondims){
    flaglevels <- unique(ref[[dim]])
    dt[get(dim) %notin% flaglevels, (flag) := 1L]
  }

  for(dim in new){
    total <- find_total(dt, dim)
    if(!is.na(total)){
      dt[get(dim) != total & get(flag) == 0, (flag) := 1L]
    } else {
      dt[get(flag) == 0, (flag) := 1L]
    }
  }

  return(dt)
}

#' @keywords internal
#' @description
#' Selects first available from MEIS > RATE > SMR > MALTALL.
#' - If cube.old = NULL, the outlier is selected from colinfo$vals.new
#' - If cube.old is provided, outlier is selected from colinfo$commoncols,
#' to ensure the same value column is used for both files.
select_outlier_pri <- function(cube.new,
                               cube.old = NULL,
                               colinfo){
  if(is.null(cube.new)) stop("'cube.new' must be provided")
  valuecolumns <- ifelse(is.null(cube.old),
                         colinfo$vals.new,
                         colinfo$commonvals)
  outlier <- data.table::fcase("MEIS" %in% valuecolumns, "MEIS",
                               "RATE" %in% valuecolumns, "RATE",
                               "SMR" %in% valuecolumns, "SMR",
                               "MALTALL" %in% valuecolumns, "MALTALL",
                               default = NA)
  return(outlier)
}

#' @title flag_outliers
#' @description
#' Adds information related to outliers, defined as values outside the interval:
#' 'weighted 25.percentile - 1.5IQR, weighted 75.percentile + 1.5IQR'
#'
#' Also adds change value (if > 1 unique year) and change outliers.
#'
#' @param cube data file
#' @param outlierval Which value is used to detect outliers? Selected with [qualcontrol::select_outlier_pri()]
#'
#' @return dt
#' @export
#'
#' @examples
flag_outliers <- function(cube,
                          outlierval){
  dt <- data.table::copy(cube)
  split_kommuneniv(dt)
  dims <- grep("^AAR$", names(dt)[names(dt) %in% .validdims], invert = T, value = T)
  keyvars <- c(dims, "AAR")
  data.table::setkeyv(dt, keyvars)

  dt <- add_outlier(dt, val = outlierval, by = dims, change = FALSE)
  dt <- add_changeval(dt, val = outlierval, by = dims)
  dt <- add_outlier(dt, val = outlierval, by = dims, change = TRUE)
  return(dt)
}

#' @title add_changeval
#' @description
#' Adds a change varible for a selected value columns. Change is defined as value/last value, with
#' last observation carried forward to get the change from last non-missing value within the strata.
#'
#' Before calculating the change score, the data is sorted by the by columns and "AAR" to get the correct
#' order within each strata.
#'
#' If < 2 unique years in the data file, nothing is done.
#' @keywords internal
#' @param dt data file ordered by
#' @param val value column to calculate change from
#' @param by dimensions to group the change value by
add_changeval <- function(dt,
                          val,
                          by){
  if(length(unique(dt$AAR)) < 2) return(dt)
  if(!all.equal(data.table::key(dt), c(by, "AAR"))) data.table::setkeyv(dt, c(by, "AAR"))
  changevalue <- paste0("change_", val)
  g <- collapse::GRP(dt, by)

  dt[, (changevalue) := collapse::flag(dt[[val]], g = g)]
  dt[, (changevalue) := zoo::na.locf(get(changevalue), na.rm = F), by = by]
  dt[, (changevalue) := 100*(get(val)/get(changevalue)-1)]
  return(dt)
}

#' @title add_outlier
#' @description
#' Add weighted quantiles and outlier information.
#'
#' If change data is requested and there are < 2 unique years in the data file, nothing is done.
#' @keywords internal
#' @param dt data file
#' @param val the value used for outlier detection
#' @param by All dimensions except AAR
#' @param change TRUE/FALSE, should outliers be detected based on the original or the change value
#' @return dt
add_outlier <- function(dt,
                        val,
                        by,
                        change = FALSE){

  if(isTRUE(change) && length(unique(dt$AAR)) < 2) return(dt)
  if(isTRUE(change)) val <- paste0("change_", val)

  by <- sub("^GEO$", "GEOniv", by)
  g <- collapse::GRP(dt, by)
  w <- dt$WEIGHTS

  cutoffs <- collapse::fmutate(
    g[["groups"]],
    MIN = collapse::fmin(dt[[val]], g = g),
    wq25 = collapse::fnth(dt[[val]], n = 0.25, g = g, w = w, ties = 1),
    wq50 = collapse::fnth(dt[[val]], n = 0.50, g = g, w = w, ties = 1),
    wq75 = collapse::fnth(dt[[val]], n = 0.75, g = g, w = w, ties = 1),
    MAX = collapse::fmax(dt[[val]], g = g),
    LOW = wq25 - 1.5*(wq75-wq25),
    HIGH = wq75 + 1.5*(wq75-wq25),
    OUTLIER = NA_integer_
  )

  lowcutoff <- "LOW"
  highcutoff <- "HIGH"
  outliercol <- "OUTLIER"

  if(isTRUE(change)){
    namechange <- names(cutoffs)[-which(names(cutoffs) %in% by)]
    data.table::setnames(cutoffs, namechange, paste0("change_", namechange))
    lowcutoff <- paste0("change_", lowcutoff)
    highcutoff <- paste0("change_", highcutoff)
    outliercol <- paste0("change_", outliercol)
  }

  dt <- collapse::join(dt, cutoffs, on = by, overid = 0, verbose = 0)
  dt[get(val) > get(highcutoff), (outliercol) := 1]
  dt[get(val) < get(lowcutoff), (outliercol) := 1]
  dt[get(val) > get(lowcutoff) & get(val) < get(highcutoff), (outliercol) := 0]

  return(dt)
}
