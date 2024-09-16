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

  misc_cols <- c("origgeo", "GEOniv", "KOMMUNE", "WEIGHTS")
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
get_cubename <- function(cube){
  gsub("^QC_|_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}|\\.csv$", "", attributes(cube)$Filename)
}

#' @keywords internal
#' @noRd
get_cubedatetag <- function(cube){
  gsub(".*(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}).*", "\\1", attributes(cube)$Filename)
}

#' @title generate_folders
#' @keywords internal
#' @description
#' Create Folder structure according to profile year and cube name of new data file
#'
#' @param cubename name of cube
#' @param year profileyear, defaults to options(qualcontrol.year)
generate_qcfolders <- function(cubename,
                               year = NULL){

  if(is.null(year)) year <- getOption("qualcontrol.year")
  path <- file.path(getOption("qualcontrol.root"),
                    getOption("qualcontrol.output"),
                    year)

  if(!dir.exists(path)) dir.create(path)

  cubedir <- file.path(path, cubename)
  filedumpdir <- file.path(cubedir, "FILDUMPER")
  plotdir <- file.path(cubedir, "PLOTT")
  bpdir <- file.path(plotdir, "Boxplot")
  bpcdir <- file.path(plotdir, "Boxplot_change")
  tsdir <- file.path(plotdir, "TimeSeries")
  tscdir <- file.path(plotdir, "TimeSeries_change")
  tsbdir <- file.path(plotdir, "TimeSeries_bydel")
  tsldir <- file.path(plotdir, "TimeSeries_country")
  tsdiffdir <- file.path(plotdir, "Diff_timetrends")

  folders <- c(cubedir,
               filedumpdir,
               plotdir,
               bpdir,
               bpcdir,
               tsdir,
               tscdir,
               tsbdir,
               tsldir,
               tsdiffdir)

  for(i in folders){
    if(!dir.exists(i)) dir.create(i)
    if(!dir.exists(file.path(i, "arkiv"))) dir.create(file.path(i, "arkiv"))
    }
}

#' @title get_all_combinations
#' @keywords internal
#' @description
#' Find all combinations of unique levels of selected columns in a dataset.
#' Can be used for rectangularization, as it returns a data.table with all possible
#' combinations.
#'
#' @param data a data.table
#' @param columns character vector of the columns to identify combinations
#'
#' @return data.table containing all possible combinations of selected variables
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
#' @noRd
#' @export
select_teller_pri <- function(valuecolumns){

  teller <- data.table::fcase("sumTELLER_uprikk" %in% valuecolumns, "sumTELLER_uprikk",
                              "sumTELLER" %in% valuecolumns, "sumTELLER",
                              "TELLER_uprikk" %in% valuecolumns, "TELLER_uprikk",
                              "TELLER" %in% valuecolumns, "TELLER",
                              default = NA_character_)
  return(teller)
}

#' @keywords internal
#' @description
#' Selects first available from sumTELLER_uprikk > sumTELLER > TELLER
#' @noRd
#' @export
select_nevner_pri <- function(valuecolumns){

  nevner <- data.table::fcase("sumNEVNER_uprikk"  %in% valuecolumns, "sumNEVNER_uprikk",
                              "sumNEVNER" %in% valuecolumns, "sumNEVNER",
                              "NEVNER_uprikk" %in% valuecolumns, "NEVNER_uprikk",
                              "NEVNER" %in% valuecolumns, "NEVNER",
                              default = NA_character_)
  return(nevner)
}

#' @title find_total
#' @keywords internal
#' @description
#' Identifies if a total/aggregated level exist for a dimension. For ALDER,
#' the min_max is used as total if existing, for other dimensions 0 is the total value.
#' @param cube datafile
#' @param dim dimension name
#' @return the total category, or NA
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
#' @keywords internal
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
#' @keywords internal
#' @description
#' Wrapper around [aggregate_cube()] to apply to multiple dimensions
#' #'
#' @param cube data file
#' @param dimensions vector of dimensions to aggregate by
#'
#' @return data.table
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
#' If filter = "new", rows with new levels are removed from cube.new.
#' If filter = "old", rows with expired levels are removed from cube.old
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



#' @title split_kommuneniv
#' @description
#' Splits GEOniv = K into K (WEIGHTS > 10000) and k (WEIGHTS < 10000). Used for plotting purposes.
#' @keywords internal
#' @noRd
split_kommuneniv <- function(data){
  if(all(c("GEOniv", "WEIGHTS") %in% names(data))){
    data[, let(GEOniv = forcats::fct_expand(GEOniv, "k", after = which(levels(GEOniv) == "K")))]
    data[GEOniv == "K" & WEIGHTS < 10000, let(GEOniv = "k")]
    levels <- c("L", "H", "F")
  }
  return(data)
}

#' @keywords internal
#' @noRd
translate_geoniv <- function(dt){
  if("GEOniv" %notin% names(dt)) stop("GEOniv column not present")
  dt <- data.table::copy(dt)[, let(GEOniv = factor(data.table::fcase(GEOniv == "L", "Land",
                                                                     GEOniv == "F", "Fylke",
                                                                     GEOniv == "K", "Kommune",
                                                                     GEOniv == "k", "Kommune",
                                                                     GEOniv == "B", "Bydel",
                                                                     GEOniv == "V", "Levekaar"),
                                                   levels = c("Land", "Fylke", "Kommune", "Bydel", "Levekaar")))]
  dt[, let(GEOniv = forcats::fct_drop(GEOniv))]

  return(dt)
}

#' @title get_complete_strata
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

#' @title update_qcyear
#' @param year corresponding to which output folder you want the output to go into.
#' Default value is given in the config_qualcontrol.yml file.
#' @export
#' @examples
#' # To use 2025 as the base folder for output:
#' # update_qcyear(year = 2025)
#' # To use the year as defined in the config file:
#' # update_qcyear(year = NULL)
update_qcyear <- function(year = NULL){
  if(is.null(year)) year <- yaml::yaml.load_file(paste("https://raw.githubusercontent.com/helseprofil/config/main/config-qualcontrol.yml"))$year
  options(qualcontrol.year = year)
}

#' @keywords internal
#' @noRd
get_plotsavefolder <- function(cubename,
                               plotfolder = c("Boxplot", "Boxplot_change", "TimeSeries", "TimeSeries_change", "TimeSeries_bydel", "TimeSeries_country", "Diff_timetrends")){
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

#' @title create_empty_standard_dt
#' @keywords internal
#' @noRd
#' @description
#' Initiates an empty data.table with the standard dimension and value columns, mainly for testing purposes.
create_empty_standard_dt <- function(){
  vals <- c(.standarddimensions, .standardvalues)
  x <- as.list(setNames(rep(NA_character_, length(vals)), vals))
  data.table::setDT(x)
  return(x)
}

#' @title qc_round
#' @keywords internal
#' @noRd
qc_round <- function(dt){
  if(is.null(dt)) return(invisible(NULL))

  dt <- data.table::copy(dt)
  values <- names(dt)[names(dt) %notin% .validdims]
  round0 <- values[grepl("SPVFLAGG.*|RATE\\.n.*", values)]
  round1 <- values[grepl("TELLER|NEVNER", values) & !grepl("_reldiff", values)]
  round2 <- values[grepl("RATE|SMR|MEIS|MIN$|MAX$|LOW$|HIGH$|.*wq\\d{2}$", values, perl = T) | grepl("_reldiff", values)]

  for(val in round0){ dt[, (val) := round(get(val), 0)] }
  for(val in round1){ dt[, (val) := round(get(val), 1)] }
  for(val in round2){ dt[, (val) := round(get(val), 2)] }

  return(dt)
}


