#' @title check_censoring
#' @description
#' Fetches censoring limits from ACCESS, and check if all values <= the censoring
#' limit has been removed. If ok, the function returns a confirmation.
#' If any observation below or equal to the limit is detected,
#' all rows containing unacceptable values are returned for inspection.
#'
#' @param data cube file
#'
#' @export
check_censoring <- function(data){

  cubename <- get_cubename(data)
  colinfo <- identify_coltypes(data)

  # Find censoring limits from ACCESS
  con <- ConnectKHelsa()
  on.exit(RODBC::odbcClose(con), add = T)

  limits <- RODBC::sqlQuery(con, paste0("SELECT PRIKK_T, PRIKK_N, Stata_PRIKK_T, Stata_PRIKK_N FROM KUBER WHERE KUBE_NAVN=",
                                        SQLstring(cubename)))
  if(nrow(limits) == 0){
    stop("KUBE ", cubename, " not found in ACCESS::KUBER")
  }

  lim_teller <- ifelse(is.na(limits$Stata_PRIKK_T), limits$PRIKK_T, limits$Stata_PRIKK_T)
  lim_nevner <- ifelse(is.na(limits$Stata_PRIKK_N), limits$PRIKK_N, limits$Stata_PRIKK_N)
  if(is.na(lim_teller) && is.na(lim_nevner)){
    cat("No censor-limits in ACCESS, is the cube uncensored?")
    return(invisible(NULL))
  }

  val_teller <- select_teller_pri(colinfo$vals.new)
  val_nevner <- select_nevner_pri(colinfo$vals.new)

  # Check censoring on TELLER
  if(is.na(val_teller)){
    cat("\nTELLER variable not available in cube, censoring on TELLER not controlled")
    notcensored_teller <- NULL
  } else {
    cat(paste0("\nTELLER variable controlled: ", val_teller))
    cat(paste0("\nCriteria: No values <= ", lim_teller))
    notcensored_teller <- data[SPVFLAGG == 0 & get(val_teller) <= lim_teller]
  }

  if(!is.null(notcensored_teller)){
    if(nrow(notcensored_teller) == 0) cat("\nNo TELLER values <= limit")
    if(nrow(notcensored_teller) > 0){
      cat(paste0("\nN values <= limit: ", nrow(notcensored_teller)))
      cat(paste0("\nView all rows with ", val_teller, " <= ", lim_teller, " with View(notcensored_teller)"))
      notcensored_teller <<- notcensored_teller
      View(notcensored_teller)
    }
  }

  # Check censoring on NEVNER
  cat("\n---")
  if(is.na(val_nevner)){
    cat("\nNEVNER variable not available in cube, censoring on NEVNER not controlled")
    notcensored_nevner <- NULL
  } else {
    cat(paste0("\nNEVNER variable controlled: ", val_nevner))
    cat(paste0("\nCriteria: No values <= ", lim_nevner))
    notcensored_nevner <- data[SPVFLAGG == 0 & get(val_nevner) <= lim_nevner]
  }

  if(!is.null(notcensored_nevner)){
    if(nrow(notcensored_nevner) == 0) cat("\nNo NEVNER values <= limit")
    if(nrow(notcensored_nevner) > 0){
      cat(paste0("\nN values <= limit: ", nrow(notcensored_nevner)))
      cat(paste0("\nView all rows with ", val_nevner, " <= ", lim_nevner, " with View(notcensored_nevner)"))
      notcensored_nevner <<- notcensored_nevner
      View(notcensored_nevner)
    }
  }
}


#' @@title compare_censoring
#' @description
#' Calculate number of censored observations and calculate absolute and relative difference
#'
#' @param cube.new new KUBE file
#' @param cube.old old KUBE file or NULL
#' @param filter.cubes Should the cubes be filtered for better comparison?
#'
#' @return a table containing the number of censored rows in the new and old KUBE, and the absolute and relative difference, grouped by type of SPVFLAGG and an additional dimension (optional)
#' @export
compare_censoring <- function(cube.new,
                              cube.old = NULL,
                              filter.cubes = TRUE){

  if(is.null(cube.new)) stop("cube.new must be provided")

  # If only new file available (new indicator), return table of new file
  if (is.null(cube.old)) {
    output <- cube.new[, .("N (new)" = .N), keyby = "SPVFLAGG"]
    convert_coltype(output, "SPVFLAGG", "factor")
    return(tab_output(output,
                      nosearchcolumns = grep("SPVFLAGG", names(output), invert = T, value = T)))
  }

  colinfo <- identify_coltypes(cube.new, cube.old)
  diminfo <- compare_dimensions(cube.new, cube.old)

  if(filter.cubes){
    cube.new <- filter_cube(cube.new, cube.old, diminfo, "new")
    cube.old <- filter_cube(cube.new, cube.old, diminfo, "old")

    for(dim in colinfo$expdims){cube.old <- aggregate_cube(cube.old, dim)}
    for(dim in colinfo$newdims){cube.new <- aggregate_cube(cube.new, dim)}
  }

  new <- cube.new[, .("N (new)" = .N), keyby = "SPVFLAGG"]
  old <- cube.old[, .("N (old)" = .N), keyby = "SPVFLAGG"]
  output <- collapse::join(new, old, on = "SPVFLAGG", how = "left", verbose = 0)

  cols <- c("N (new)", "N (old)")
  output[, (cols) := lapply(.SD, collapse::replace_na, 0), .SDcols = cols]

  output[, let(Absolute = `N (new)` - `N (old)`,
               Relative = round(`N (new)` / `N (old)`, 3))]

  convert_coltype(output, "SPVFLAGG", "factor")
  return(tab_output(output,
                    nosearchcolumns = grep("SPVFLAGG", names(output), invert = T, value = T)))

}

#' @title compare_censoring_timeseries
#' @description
#' Compare censored observations within each time series. List number and proportion
#' of time series with 0, 1, 2, ..., n censored observations.
#'
#' @param cube.new new file
#' @param cube.old old file
#'
#' @export
compare_censoring_timeseries <- function(cube.new,
                                         cube.old = NULL){

  if(is.null(cube.new)) stop("cube.new must be provided")

  colinfo <- identify_coltypes(cube.new, cube.old)

  if(is.null(cube.old)){
    d <- data.table::copy(cube.new)
    groupdims <- grep("^AAR$", colinfo$dims.new, invert = T, value = T)
    d <- d[, .(N_censored = sum(SPVFLAGG != 0, na.rm = T)), by = groupdims]
    d <- d[, .N, by = N_censored]
    d[, let(Proportion = paste(round(100*N/sum(N), 1), "%"))]
  }

  if(!is.null(cube.old)){
    d <- data.table::rbindlist(list(data.table::copy(cube.new)[!grepl("99$", GEO), mget(colinfo$commoncols)][, cube := "New"],
                                    data.table::copy(cube.old)[!grepl("99$", GEO), mget(colinfo$commoncols)][, cube := "Old"]))

    groupdims <- grep("^AAR$", c(colinfo$commondims), invert = T, value = T)
    d <- d[, .(N_censored = sum(SPVFLAGG != 0, na.rm = T)), by = c("cube", groupdims)]
    d <- d[, .N, by = .(cube, N_censored)]
    d[, let(Proportion = paste(round(100*N/sum(N), 1), "%")), by = cube]
    d <- data.table::dcast(d, N_censored~cube, value.var = c("N", "Proportion"))[order(N_censored)]
    data.table::setcolorder(d, c("N_censored",
                                 grep("New", names(d), value = T),
                                 grep("Old", names(d), value = T)))
    convert_coltype(d, "N_censored", "factor")
    data.table::setnames(d, names(d), gsub("_", " ", names(d)))
  }

  return(tab_output(d, nosearchcolumns = 2:ncol(d) - 1))
}
