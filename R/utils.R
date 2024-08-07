#' @title identify_coltypes
#' @description
#' Uses internal object .validdims to identify dimension and value columns
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
  gsub("^QC_|_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}|\\.csv$", "", attributes(data)$Filename)
}

#' @title generate_folders
#' @description
#' Create Folder structure according to profile year and cube name of new data file
#'
#' @param profileyear profileyear
#' @param kubename name of cube
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

#' @keywords internal
#' @description
#' Selects first available from sumTELLER_uprikk > sumTELLER > TELLER
#'
#' @noRd
select_teller_pri <- function(valuecolumns){

  teller <- data.table::fcase("sumTELLER_uprikk" %in% valuecolumns, "sumTELLER_uprikk",
                              "sumTELLER" %in% valuecolumns, "sumTELLER",
                              "TELLER" %in% valuecolumns, "TELLER",
                              default = NA_character_)
  return(teller)
}

#' @keywords internal
#' @description
#' Selects first available from sumTELLER_uprikk > sumTELLER > TELLER
#'
#' @noRd
select_nevner_pri <- function(valuecolumns){

  nevner <- data.table::fcase("sumNEVNER_uprikk"  %in% valuecolumns, "sumNEVNER_uprikk",
                              "sumNEVNER" %in% valuecolumns, "sumNEVNER",
                              "NEVNER" %in% valuecolumns, "NEVNER",
                              default = NA_character_)
  return(nevner)
}

#' @title find_total
#' @description
#' Identifies if a total/aggregated level exist for a dimension. For ALDER,
#' the min_max is used as total if existing, for other dimensions 0 is the total value.
#' @param cube datafile
#' @param dim dimension name
#' @return the total category, or NA
#' @export
#' @examples
#' # find_total(newcube, "ALDER")
find_total <- function(cube, dim){

  if(dim != "ALDER"){
    total <- data.table::fcase(0 %in% unique(cube[[dim]]), 0,
                               default = NA)
  }

  if(dim == "ALDER"){
    ALDERlevels <- unique(cube$ALDER)
    ALDERl <- collapse::fmin(as.numeric(sub("(\\d*)_(\\d*)", "\\1", ALDERlevels)))
    ALDERh <- collapse::fmax(as.numeric(sub("(\\d*)_(\\d*)", "\\2", ALDERlevels)))
    ALDERtot <- paste0(ALDERl, "_", ALDERh)
    if(ALDERtot %in% ALDERlevels){
      total <- ALDERtot
    } else {
      total <- NA
    }
  }

  return(total)
}

#' @title aggregate_cube
#' @description
#' Aggregates value columns according to a given dimension variable.
#' If a total exists, the rows corresponding to the total is kept. If
#' no total exists, value columns will be aggregated according to their types.
#' Counts (TELLER, sumTELLER) are aggregated to the sum, while other value columns
#' are aggregated to their mean.
#'
#' @param cube cube file
#' @param dim dimension to aggregate on
aggregate_cube <- function(cube, dim){

  if(dim %in% c("AAR")) stop("cannot aggregate on 'AAR'")
  cube <- data.table::copy(cube)
  colorder <- names(cube)
  total <- find_total(cube, dim)

  if(!is.na(total)){
    return(cube[get(dim) == total])
  }

  if(is.na(total)){
    colinfo <- identify_coltypes(cube)
    vals <- colinfo$vals.new
    vals <- grep("SPVFLAGG", vals, value = T, invert = T)
    cube[, (vals) := lapply(.SD, as.numeric), .SDcols = vals]
    sumvals <- grep("TELLER", vals, value = T)
    avgvals <- grep("TELLER", vals, value = T, invert = T)
    groupdims <- grep(dim, colinfo$dims.new, value = T, invert = T)
    data.table::setkeyv(cube, groupdims)

    cube[, (avgvals) := lapply(.SD, mean, na.rm = T), .SDcols = avgvals, by = groupdims]
    cube[, (sumvals) := lapply(.SD, sum, na.rm = T), .SDcols = sumvals, by = groupdims]
    for(i in avgvals){cube[is.nan(get(i)), (i) := NA_real_]}
    cube[, (dim) := "Total"]
    cube <- cube[, .SD[1], by = groupdims]
    data.table::setcolorder(cube, colorder)
    return(cube)
  }
}

#'
#' @title filter_cube
#' @description
#' Filters out new and expired levels in new and old file, respectively, for comparison.
#'
#' @param cube.new new file
#' @param cube.old old file
#' @param dimtable table generated with [qualcontrol::compare_dimensions()]
#' @param filter "new" or "old", indicating whether the file to filter is the new or old file
filter_cube <- function(cube.new,
                        cube.old,
                        dimtable,
                        filter = c("new", "old")){
  filter <- match.arg(filter)
  filteron <- switch(filter,
                     new = "New levels",
                     old = "Expired levels")
  filtercube <- switch(filter,
                       new = data.table::copy(cube.new),
                       old = data.table::copy(cube.old))
  refcube <- switch(filter,
                    new = data.table::copy(cube.old),
                    old = data.table::copy(cube.new))
  filterdims <- data.table::copy(dimtable)[get(filteron) != ""]$Dimension

  if(length(filterdims) > 0){
    for(dim in filterdims){
      filtercube <- filtercube[get(dim) %in% unique(refcube[[dim]])]
    }
  }

  return(filtercube)
}

#' @keywords internal
#' @noRd
convert_coltype <- function(data,
                            columns,
                            format = c("factor", "numeric", "integer", "character")){

  format <- match.arg(format)
  as.numeric.qc <- function(x){
    if(is.factor(x) && !any(grepl("[^0-9]", levels(x)))){
      as.numeric(as.character(x))
    } else {
     as.numeric(x)
    }
  }

  fun <- switch(format,
                factor = as.factor,
                numeric = as.numeric.qc,
                integer = as.integer,
                character = as.character)

  data[, (columns) := lapply(.SD, fun), .SDcols = columns]
}
