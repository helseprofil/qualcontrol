#' @title make_comparecube
#'
#' @param cube.new new file, default = newcube
#' @param cube.old old file, default = oldcube. Can be NULL.
#' @param outliers should outliers be flagged using [qualcontrol::flag_outliers()]?
#' @param dumps which files to save, defaults to getOption("qualcontrol.dumps"). For no dumps, set to NULL.
#' @param overwrite should existing files (exact filename) be overwritten?
#'
#' @returns flagged cube objects, comparecube, and saved csv-files if requested.
#' @export
make_comparecube <- function(cube.new = newcube,
                             cube.old = oldcube,
                             outliers = TRUE,
                             dumps = getOption("qualcontrol.dumps")){

  newcube_flag <- oldcube_flag <- comparecube <- outlierval <- NULL
  colinfo <- identify_coltypes(cube.new, cube.old)
  if(outliers) outlierval <- select_outlier_pri(cube.new, cube.old, colinfo)

  newcube_flag <- flag_rows(cube.new, cube.old, colinfo, "newrow")
  if(outliers && !is.na(outlierval)){
    newcube_flag <- flag_outliers(newcube_flag, outlierval)
    data.table::setattr(newcube_flag, "outlier", outlierval)
  }

  if(!is.null(cube.old)){
    oldcube_flag <- flag_rows(cube.new, cube.old, colinfo, "exprow")
    if(outliers && !is.na(outlierval)){
      oldcube_flag <- flag_outliers(oldcube_flag, outlierval)
      add_prev_outlier(newcube_flag, oldcube_flag, colinfo)
      data.table::setattr(oldcube_flag, "outlier", outlierval)
      data.table::setattr(newcube_flag, "comparison", get_cubefilename(cube.old))
    }
    comparecube <- combine_cubes(newcube_flag, oldcube_flag, colinfo)
  }

  newcube_flag <<- newcube_flag
  oldcube_flag <<- oldcube_flag
  comparecube <<- comparecube

  if(!is.null(dumps)){
    generate_qcfolders(get_cubename(cube.new), year = getOption("qualcontrol.year"))
    savepath <- get_dump_folder(cubename = get_cubename(newcube_flag))
    archive_old_files(savepath, ".csv")
    for(dump in dumps){
      save_dump(dump,
                newcube_flag = newcube_flag,
                oldcube_flag = oldcube_flag,
                comparecube = comparecube,
                path = savepath)
    }
  }
}

#' @title select_outlier_pri
#' @keywords internal
#' @noRd
#' @description
#' Selects first available from MEIS > RATE > SMR > MALTALL.
#' - If cube.old = NULL, the outlier is selected from colinfo$vals.new
#' - If cube.old is provided, outlier is selected from colinfo$commoncols,
#' to ensure the same value column is used for both files.
#' @export
select_outlier_pri <- function(cube.new,
                               cube.old = NULL,
                               colinfo){
  if(is.null(cube.new)) stop("'cube.new' must be provided")
  onlynew <- ifelse(is.null(cube.old), "yes", "no")

  valuecolumns <- switch(onlynew,
                         yes = colinfo$vals.new,
                         no = colinfo$commonvals)

  outlier <- data.table::fcase("MEIS" %in% valuecolumns, "MEIS",
                               "RATE" %in% valuecolumns, "RATE",
                               "SMR" %in% valuecolumns, "SMR",
                               "MALTALL" %in% valuecolumns, "MALTALL",
                               default = NA)
  return(outlier)
}

#' @title flag_rows
#' @keywords internal
#' @description
#' Flags the new data file for new rows (not in the old file), and the old data file for expired rows (not in the new file).
#' @param cube.new new data file
#' @param cube.old old data file
#' @param colinfo list generated with [qualcontrol::identify_coltypes(cube.new, cube.old)]
#' @param flag one of "newrow" or "exprow", corresponding to which type of column should be flagged.
#'
#' @return flagged data file
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

#' @title flag_outliers
#' @keywords internal
#' @noRd
#' @description
#' Adds information related to outliers, defined as values outside the interval:
#' 'weighted 25.percentile - 1.5IQR, weighted 75.percentile + 1.5IQR'
#' Also adds change value (if > 1 unique year) and change outliers.
#' @param cube data file
#' @param outlierval Which value is used to detect outliers? Selected with [qualcontrol::select_outlier_pri()]
#' @return cube with outlier information
flag_outliers <- function(cube,
                          outlierval){
  dt <- data.table::copy(cube)
  split_kommuneniv(dt)
  dims <- grep("^AAR$", names(dt)[names(dt) %in% getOption("qualcontrol.alldimensions")], invert = T, value = T)
  keyvars <- c(dims, "AAR")
  data.table::setkeyv(dt, keyvars)

  dt <- add_outlier(dt, val = outlierval, by = dims, change = FALSE)
  dt <- add_changeval(dt, val = outlierval, by = dims)
  dt <- add_outlier(dt, val = outlierval, by = dims, change = TRUE)
  return(dt)
}

#' @title add_changeval
#' @keywords internal
#' @noRd
#' @description
#' Adds a change varible for a selected value columns. Change is defined as value/last value, with
#' last observation carried forward to get the change from last non-missing value within the strata.
#'
#' Before calculating the change score, the data is sorted by the by columns and "AAR" to get the correct
#' order within each strata.
#'
#' If < 2 unique years in the data file, nothing is done.
#'
#' To avoid change values of Inf, when the previous value was 0, this is replaced with
#' half of the minimum non-zero value in the data set
#' @param dt data file ordered by
#' @param val value column to calculate change from
#' @param by dimensions to group the change value by
add_changeval <- function(dt,
                          val,
                          by){
  if(length(unique(dt$AAR)) < 2) return(dt)
  if(!all.equal(data.table::key(dt), c(by, "AAR"))) data.table::setkeyv(dt, c(by, "AAR"))
  changevalue <- paste0("change_", val)
  min_nonzeroval <- dt[get(val) != 0, collapse::fmin(get(val))]
  g <- collapse::GRP(dt, by)

  dt[, (changevalue) := collapse::flag(dt[[val]], g = g)]
  dt[, (changevalue) := zoo::na.locf(get(changevalue), na.rm = F), by = by]
  dt[get(changevalue) == 0, (changevalue) := min_nonzeroval/2]
  dt[, (changevalue) := 100*(get(val)/get(changevalue)-1)]
  return(dt)
}

#' @title add_outlier
#' @keywords internal
#' @noRd
#' @description
#' Add weighted quantiles and outlier information.
#'
#' If change data is requested and there are < 2 unique years in the data file, nothing is done.
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

  dt <- collapse::join(dt, cutoffs, on = by, verbose = 0, overid = 2)
  dt[get(val) > get(highcutoff), (outliercol) := 1]
  dt[get(val) < get(lowcutoff), (outliercol) := 1]
  dt[get(val) > get(lowcutoff) & get(val) < get(highcutoff), (outliercol) := 0]

  return(dt)
}

#' @title add_prev_outlier
#' @keywords internal
#' @noRd
#' @description
#' Adds information regarding new/old outliers to newcube_flag.
#' @param newcube_flag newcube_flag
#' @param oldcube_flag oldcube_flag
#' @param colinfo list generated with [qualcontrol::identify_coltypes(cube.new, cube.old)]
#' @return newcube_flag with previous outlier column
add_prev_outlier <- function(newcube_flag,
                             oldcube_flag,
                             colinfo){

  newcube_flag[oldcube_flag, let(PREV_OUTLIER = i.OUTLIER), on = colinfo[["commondims"]]]
  newcube_flag[, let(NEW_OUTLIER = 0L)]
  newcube_flag[OUTLIER == 1 & (is.na(PREV_OUTLIER | PREV_OUTLIER == 0)), let(NEW_OUTLIER = 1L)]

  if("change_OUTLIER" %in% names(newcube_flag) && "change_OUTLIER" %in% names(oldcube_flag)){
    newcube_flag[oldcube_flag, let(change_PREV_OUTLIER = i.change_OUTLIER), on = colinfo[["commondims"]]]
    newcube_flag[, let(change_NEW_OUTLIER = 0L)]
    newcube_flag[change_OUTLIER == 1 & (is.na(change_PREV_OUTLIER | change_PREV_OUTLIER == 0)), let(change_NEW_OUTLIER = 1L)]
  }

  data.table::setcolorder(newcube_flag, c(grep("change_", names(newcube_flag), invert = T, value = T),
                                          grep("change_", names(newcube_flag), value = T)))

}

#' @title combine_cubes
#' @keywords internal
#' @noRd
#' @param newcube_flag newcube_flag generated by [qualcontrol::flag_rows()] and [qualcontrol::flag_outliers]
#' @param oldcube_flag oldcube_flag generated by [qualcontrol::flag_rows()] and [qualcontrol::flag_outliers]
#' @param colinfo list generated with [qualcontrol::identify_coltypes(cube.new, cube.old)]
#'
#' @return comparecube
combine_cubes <- function(newcube_flag,
                          oldcube_flag,
                          colinfo){

  d_new <- data.table::copy(newcube_flag)
  d_old <- data.table::copy(oldcube_flag)[exprow == 0]
  commonvals <- colinfo$commonvals
  valuecolumns <- c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER", "RATE.n")

  for(col in valuecolumns){
    if(col %in% names(d_old) & !(col %in% names(d_new)) & paste0(col, "_uprikk") %in% names(d_new)){
      d_new[, (col) := get(paste0(col, "_uprikk"))]
      d_new[SPVFLAGG != 0, (col) := NA_real_]
      commonvals <- c(commonvals, col)
    }
  }

  d_new <- d_new[, c(..colinfo[["commondims"]], ..commonvals, "newrow", "GEOniv")]
  data.table::setnames(d_new, commonvals, paste0(commonvals, "_new"))

  # Handle new (add total to d_old) and expired (aggregate d_old) dimensions
  if(length(colinfo$expdims) > 0) aggregate_cube_multi(d_old, colinfo$expdims)
  if(length(colinfo$newdims) > 0) {
    for(dim in colinfo$newdims){
      d_old[, (dim) := find_total(d_new, dim)]
    }
  }

  d_old <- d_old[, c(..colinfo[["commondims"]], ..commonvals)]
  data.table::setnames(d_old, commonvals, paste0(commonvals, "_old"))

  compare <- collapse::join(d_new, d_old, on = colinfo[["commondims"]], how = "full", verbose = 0, overid = 2)

  colorder <- c("GEOniv", "newrow", colinfo[["commondims"]])
  for(val in commonvals){
    colorder <- c(colorder, paste0(val, c("_new", "_old")))
  }

  data.table::setcolorder(compare, colorder)
  add_diffcolumns(compare, commonvals)
  return(compare)
}

#' @title add_diffcolumns
#' @keywords internal
#' @noRd
#' @description
#' adds columns with relative and absolute differences between new and old cube by reference,
#' no need to overwrite object.
#' @param comparecube combined new and old cube with _new and _old valuecolumns, created by [qualcontrol::combine_cubes]
#' @param valuecolumns vector containing value columns to calculate diff columns
#' @return comparecube with diff columns
add_diffcolumns <- function(comparecube,
                            valuecolumns){

  for(val in valuecolumns){
    new <- paste0(val, "_new")
    old <- paste0(val, "_old")
    diff <- paste0(val, "_diff")
    reldiff <- paste0(val, "_reldiff")
    comparecube[, (diff) := get(new) - get(old)]
    comparecube[, (reldiff) := get(new) / get(old)]
    # For rows with missing new or old values, set _diff and _reldiff to NA
    comparecube[is.na(get(new)) + is.na(get(old)) == 1, (diff) := NA_real_]
    comparecube[is.na(get(new)) + is.na(get(old)) == 1, (reldiff) := NA_real_]
    # For rows with missing old AND new, set _diff = 0, and _reldiff = 1
    comparecube[is.na(get(new)) & is.na(get(old)), (diff) := 0]
    comparecube[is.na(get(new)) & is.na(get(old)), (reldiff) := 1]
  }

  for(val in c("SPVFLAGG", "RATE.n")){
    if(val %in% valuecolumns){
      delete <- paste0(val, "_reldiff")
      comparecube[, (delete) := NULL]
    }
  }

  diffcolumns <- grep("_diff$", names(comparecube), value = T)
  comparecube[, let(any_diffs = 0L)]
  comparecube[rowSums(abs(comparecube[, ..diffcolumns]) > 0.1, na.rm = T) > 0, let(any_diffs = 1L)]
}

#' @title get_dump_folder
#' @keywords internal
#' @noRd
#' @description
#' Helper function to save file dumps created by [qualcontrol::make_comparecube()]. Generate file path.
get_dump_folder <- function(cubename){
  file.path(getOption("qualcontrol.root"),
            getOption("qualcontrol.output"),
            getOption("qualcontrol.year"),
            cubename,
            "FILDUMPER")
}

#' @title save_dump
#' @keywords internal
#' @noRd
#' @description
#' Helper function to save file dumps created by [qualcontrol::make_comparecube()]
save_dump <- function(dump,
                      newcube_flag,
                      oldcube_flag,
                      comparecube,
                      path){

  file <- switch(dump,
                 "newcube_flag" = qc_round(newcube_flag),
                 "oldcube_flag" = qc_round(oldcube_flag),
                 "comparecube" = qc_round(comparecube))

  if(is.null(file)){
    cat("Dump: ", dump, " requested, but does not exist. No file saved.")
    return(invisible(NULL))
  }

  filename <- switch(dump,
                     "newcube_flag" = paste0("newflag_", get_cubename(newcube_flag), "_", get_cubedatetag(newcube_flag), ".csv"),
                     "oldcube_flag" = paste0("oldflag_", get_cubename(oldcube_flag), "_", get_cubedatetag(oldcube_flag), ".csv"),
                     "comparecube" = set_filename_comparecube(newcube_flag, oldcube_flag))

  savepath <- file.path(path, filename)
  data.table::fwrite(file, savepath, sep = ";")
  cat(paste0("\nFILEDUMP saved ", filename, "\n"))
}

#' @title set_filename_comparecube
#' @keywords internal
#' @noRd
#' @description
#' Helper function to save file dumps created by [qualcontrol::make_comparecube()]
set_filename_comparecube <- function(newcube_flag, oldcube_flag){

newname <- get_cubename(newcube_flag)
newdatetag <- get_cubedatetag(newcube_flag)
oldname <- get_cubename(oldcube_flag)
olddatetag <- get_cubedatetag(oldcube_flag)

fullname <- ifelse(newname == oldname,
                   paste0(newname, "_", newdatetag, "_vs_", olddatetag),
                   paste0(newname, "_", newdatetag, "_vs_", oldname, "_", olddatetag))

filename <- paste0("compare_", fullname, ".csv")
return(filename)
}

#' @title qc_round
#' @keywords internal
#' @noRd
qc_round <- function(dt){
  if(is.null(dt)) return(invisible(NULL))

  dt <- data.table::copy(dt)
  values <- names(dt)[names(dt) %notin% getOption("qualcontrol.alldimensions")]
  round0 <- values[grepl("SPVFLAGG.*|RATE\\.n.*", values)]
  round1 <- values[grepl("TELLER|NEVNER", values) & !grepl("_reldiff", values)]
  round2 <- values[grepl("RATE|SMR|MEIS|MIN$|MAX$|LOW$|HIGH$|.*wq\\d{2}$", values, perl = T) | grepl("_reldiff", values)]

  for(val in round0){ dt[, (val) := round(get(val), 0)] }
  for(val in round1){ dt[, (val) := round(get(val), 1)] }
  for(val in round2){ dt[, (val) := round(get(val), 2)] }

  return(dt)
}
