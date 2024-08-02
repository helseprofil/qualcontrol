#' @title identify_coltypes
#'
#' uses internal object .validdims to identify dimension and value columns
#'
#' @param cube.new new file
#' @param cube.old old file, or NULL
#'
#' @return
#' a list containing information on dimension and value columns
#' @export
identify_coltypes <- function(cube.new = NULL,
                              cube.old = NULL){

  if(is.null(cube.new)) stop("cube.new must be provided")

  misc_cols <- c("origgeo", "GEOniv")
  out <- list()

  out[["dims.new"]] <- names(cube.new)[names(cube.new) %in% .validdims]
  out[["vals.new"]] <- names(cube.new)[names(cube.new) %notin% c(out$dims.new, misc_cols)]
  out[["misc.new"]] <- names(cube.new)[names(cube.new) %in% misc_cols]

  if(!is.null(cube.old)){
    out[["dims.old"]] <- names(cube.old)[names(cube.old) %in% .validdims]
    out[["vals.old"]] <- names(cube.old)[names(cube.old) %notin% c(out$dims.old, misc_cols)]
    out[["misc.old"]] <- names(cube.old)[names(cube.old) %in% misc_cols]
    out[["commondims"]] <- intersect(out$dims.new, out$dims.old)
    out[["commonvals"]] <- intersect(out$vals.new, out$vals.old)
    out[["commoncols"]] <- c(out$commondims, out$commonvals)
    out[["newdims"]] <- setdiff(out$dims.new, out$dims.old)
    out[["expdims"]] <- setdiff(out$dims.old, out$dims.new)
    out[["newvals"]] <- setdiff(out$vals.new, out$vals.old)
    out[["expvals"]] <- setdiff(out$vals.old, out$vals.new)
  }
  return(out)
}


#' @keywords internal
#' @noRd
get_cubename <- function(data){
  gsub("^QC_|_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}\\.csv$", "", attributes(data)$Filename)
}

#' @title generate_folders
#'
#' Create Folder structure according to profile year and cube name of new data file
#'
#' @param profileyear
#' @param kubename
#' @export
generate_qcfolders <- function(year,
                               cubename){

  path <- file.path(getOption("qualcontrol.root"),
                    getOption("qualcontrol.output"),
                    year)

  if(!dir.exists(path)) dir.create(path)

  cubedir <- file.path(path, cubename)
  filedumpdir <- file.path(cubedir, "FILDUMPER")
  plotdir <- file.path(cubedir, "PLOTT")
  bpdir <- file.path(plotdir, "BP")
  bpcdir <- file.path(plotdir, "BPc")
  tsdir <- file.path(plotdir, "TS")
  tscdir <- file.path(plotdir, "TSc")
  tldir <- file.path(plotdir, "TL")

  folders <- c(cubedir,
               filedumpdir,
               plotdir,
               bpdir,
               bpcdir,
               tsdir,
               tscdir,
               tldir)

  for(i in folders){
    if(!dir.exists(i)) dir.create(i)
    if(!dir.exists(file.path(i, "arkiv"))) dir.create(file.path(i, "arkiv"))
    }
}

#' @title get_all_combinations
#' @description
#' Find all combinations of unique levels of selected columns in a dataset.
#' Can be used for rectangularization, as it returns a data.table with all possible
#' combinations.
#'
#' @param data a data.table
#' @param columns character vector of the columns to identify combinations
#'
#' @return data.table containing all possible combinations of selected variables
#' @export
#'
#' @examples
#' # get_all_combinations(newcube, c("KJONN", "ALDER", "UTDANN"))
get_all_combinations <- function(data,
                                 columns){

  if(any(columns %notin% names(data))){
    missing <- paste0(columns[columns %notin% names(data)], collapse = ", ")
    stop("Column(s) (", missing, ") not found in data")
  }

    d <- do.call(data.table::CJ, lapply(columns, function(x) unique(data[[x]])))
    d <- data.table::setnames(d, columns)
    return(d)
}
