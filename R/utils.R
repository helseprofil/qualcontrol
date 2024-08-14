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

  misc_cols <- c("origgeo", "GEOniv", "KOMMUNE")
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
                              "TELLER_uprikk" %in% valuecolumns, "TELLER_uprikk",
                              "TELLER" %in% valuecolumns, "TELLER",
                              default = NULL)
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
                              "NEVNER_uprikk" %in% valuecolumns, "NEVNER_uprikk",
                              "NEVNER" %in% valuecolumns, "NEVNER",
                              default = NULL)
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

#' @title aggregate_cube_multi
#' @description
#' Wrapper around [aggregate_cube()] to apply to multiple dimensions
#' #'
#' @param cube data file
#' @param dimensions vector of dimensions to aggregate by
#'
#' @return data.table
#' @export
aggregate_cube_multi <- function(cube, dimensions){
  d <- data.table::copy(cube)
  for(dim in dimensions){
    d <- aggregate_cube(d, dim)
  }
  return(d)
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

#' @keywords internal
#' @noRd
#' @description
#' Adds column GEOniv to identify geographical levels. Uses population info from "sysdata.rda",
#' stored as the internal object .popinfo
#' Adds the column by reference, no need to overwrite object.
#' @examples
#' # add_geoniv(data)
add_geoniv <- function(data,
                       combine.kommune = F){
  data[.popinfo, GEOniv := i.GEOniv, on = "GEO"]
  if(combine.kommune) data[, GEOniv := forcats::fct_collapse(GEOniv, K = c("K", "k"))]
  data[, GEOniv := forcats::fct_drop(GEOniv)]
  return(data)
}

#' @keywords internal
#' @noRd
translate_geoniv <- function(data){
  if("GEOniv" %notin% names(data)) stop("GEOniv column not present")
  data <- data.table::copy(data)[, GEOniv := data.table::fcase(GEOniv == "L", "Land",
                                                               GEOniv == "F", "Fylke",
                                                               GEOniv == "K", "Kommune",
                                                               GEOniv == "B", "Bydel")]
  return(data)
}

#' @keywords internal
#' @noRd
#' @description
#' Adds column KOMMUNE to identify Oslo, Stavanger, Bergen, and Trondheim for
#' grouping. Adds the column by reference, no need to overwrite object.
#' @examples
#' # add_commune(data)
add_kommune <- function(data){
  data[, let(KOMMUNE = NA_character_)]
  data[GEO %in% c(301, 1103, 4601, 5001) | GEOniv == "B",
       let(KOMMUNE = data.table::fcase(grepl("^301", GEO), "Oslo",
                                       grepl("^1103", GEO), "Stavanger",
                                       grepl("^4601", GEO), "Bergen",
                                       grepl("^5001", GEO), "Trondheim"))]
  return(data)
}

#' @keywords internal
#' @description
#' Returns only complete strata, as defined by the columns in `by`. Generate
#' a new column called `n_censored`, indicating the number of censored observations in
#' each strata, and returns rows where `n_censored` = 0.
#' `n_censored` is added by reference to the data file. To also filter the data, the data must be assigned, see examples.
#' @param data data file
#' @param by dimension columns to get complete strata by
#' @param type Should complete strata be identified by SPVFLAGG ("censored") or by missing value column ("missing")
#' @param valuecolumn must be provided when type = "missing", which value column should be used to identify complete strata?
#'
#' @examples
#' # Add n_censored, no filtering
#' # get_complete_strata(data, by = bycols, type = "censored")
#' # Actually filter data
#' # data <- get_complete_strata(data, by = bycols, type = "censored")
get_complete_strata <- function(data,
                                by,
                                type = c("censored", "missing"),
                                valuecolumn = NULL){
  if("GEO" %in% by) by <- grep("^GEO$", by, invert = T, value = T)

  if(type == "missing" && (is.null(valuecolumn) || valuecolumn %notin% names(data))){
     stop("When type = 'missing', a valid column name must be provided to the value argument")
  }

  switch(type,
         censored = data[, let(n_censored = sum(SPVFLAGG != 0)), by = by],
         missing = data[, let(n_censored = sum(is.na(get(valuecolumn)))), by = by])
  data <- data[n_censored == 0]
  data[, let(n_censored = NULL)]
  return(data)
}
