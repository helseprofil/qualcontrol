#' @title compare_geolevels
#' @description
#' Checks if a geographical level sums up to the next granularity.
#'
#' @param cube data file
#' @param comparison which geographical levels are compared. Accepts combinations "FL" (Fylke-Land),
#' "KF" (Kommune-Fylke"), "BK" (Bydel-Kommune), and "OO" (Oslo kommune-fylke).
#' @return DT
#' @export
#' @examples
#' # compare_geolevels(data, "FL")
#' # compare_geolevels(data, "KF")
#' # compare_geolevels(data, "BK")
#' # compare_geolevels(data, "OO")
compare_geolevels <- function(cube,
                              comparison = c("FL", "LF", "KF", "FK", "BK", "KB", "OO")){

  comparison <- match.arg(comparison)
  comparison <- paste0(sort(strsplit(comparison, "")[[1]]), collapse = "")
  d <- data.table::copy(cube)
  groupdims <- grep("^GEO$", identify_coltypes(d)$dims.new, invert = T, value = T)
  teller_val <- select_teller_pri(names(d))
  if(is.null(teller_val)){
    cat("No sumTELLER(_uprikk) or TELLER(_uprikk) available, output not generated")
    return(invisible(NULL))
  }

  # Filter correct data
  add_geoniv(d, combine.kommune = T)
  d <- switch(comparison,
              FL = d[GEOniv %in% c("L", "F")],
              FK = d[GEOniv %in% c("F", "K")],
              BK = d[GEO %in% c(301, 1103, 4601, 5001) | GEOniv == "B"],
              OO = d[GEO %in% c(3, 301)])
  if(length(unique(d$GEOniv)) < 2) stop("Only 1 of the required geolevels present")
  d <- translate_geoniv(d)
  if(comparison == "OO") d[, GEOniv := paste0("Oslo ", GEOniv)]

  outcols <- switch(comparison,
                    FL = c("Land", "Fylke"),
                    FK = c("Fylke", "Kommune"),
                    BK = c("Kommune", "Bydel"),
                    OO = c("Oslo Fylke", "Oslo Kommune"))

  if(comparison == "BK"){
    add_kommune(d)
    groupdims <- c("KOMMUNE", groupdims)
  }

  by <- c("GEOniv", groupdims)

  # Estimate sum and diffs
  d <- d[, .("sum" = collapse::fsum(get(teller_val))), keyby = by]
  d <- data.table::dcast(d, ... ~ GEOniv, value.var = "sum")
  data.table::setcolorder(d, c(groupdims, outcols[1], outcols[2]))
  d[, let(Absolute = get(outcols[1])-get(outcols[2]),
          Relative = round(get(outcols[1])/get(outcols[2]), 3))]

  # Format output
  d[, (groupdims) := lapply(.SD, as.factor), .SDcols = groupdims]
  d[, (c(outcols, "Absolute")) := lapply(.SD, round, 0), .SDcols = c(outcols, "Absolute")]
  data.table::setorder(d, -Relative, na.last = T)
  return(tab_output(d, nosearchcolumns = outcols))
}

#' @title unknown_bydel
#' @description
#' Estimate the proportion of unknown bydel, to check validity of the data.
#'
#' @param cube data file
#' @param maxrows Should the output table ble cropped to show a maximum of 4000 observations?
#'
#' @return DT
#' @export
unknown_bydel <- function(cube,
                          maxrows = TRUE){
  d <- data.table::copy(cube)
  add_geoniv(d)
  if(nrow(d[GEOniv == "B"]) == 0){
    cat("No data on bydel, no check performed")
    return(invisible(NULL))
  }

  colinfo <- identify_coltypes(d)
  tellerval <- select_teller_pri(colinfo$vals.new)
  nevnerval <- select_nevner_pri(colinfo$vals.new)
  targets <- c(tellerval, nevnerval)
  targets <- targets[!is.na(targets)]
  bydims <- grep("^GEO$", colinfo$dims.new, invert = T, value = T)

  if(length(targets) == 0){
    cat("No TELLER or NEVNER columns found in data, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  }

  targets <- targets[!is.na(targets)]

  # Filter and format data
  d <- d[GEO %in% c(301, 1103, 4601, 5001) | GEOniv == "B"]
  contains_bydel <- d[GEOniv == "B" & SPVFLAGG == 0, unique(AAR)]
  d <- d[AAR %in% contains_bydel]
  add_kommune(d)
  d <- d[, mget(c("KOMMUNE", "GEOniv", colinfo$dims.new, targets, "SPVFLAGG"))]
  d <- data.table::melt(d, measure.vars = targets, variable.name = "TARGET")
  d <- get_complete_strata(d, c("KOMMUNE", "TARGET", bydims), "missing", "value")

  if(nrow(d) < 1){
    cat("No complete strata, not possible to estimate unspecified bydel. Was bydelstart set to the correct year?\n")
    return(invisible(NULL))
  }

  # Estimate % unknown
  d <- d[, .(sum = sum(value, na.rm = T)), by = c("KOMMUNE", "GEOniv", "TARGET", bydims)]
  d <- data.table::dcast(d, ... ~ GEOniv, value.var = "sum")
  d[, UNKNOWN := round(100*(1 - B/K), 2)]
  d[B == 0 & K == 0, UNKNOWN := NA_real_]
  d <- d[order(-UNKNOWN)]
  data.table::setcolorder(d, c("KOMMUNE", bydims, "TARGET", "K", "B", "UNKNOWN"))
  data.table::setnames(d, c("K", "B", "UNKNOWN"), c("Kommune", "Bydel", "UNKNOWN, %"))

  cat(paste0("Total number of strata with complete bydel (teller): ", nrow(d[TARGET == tellerval])))
  cat(paste0("\nOslo: ", nrow(d[TARGET == tellerval & KOMMUNE == "Oslo"])))
  cat(paste0("\nBergen: ", nrow(d[TARGET == tellerval & KOMMUNE == "Bergen"])))
  cat(paste0("\nStavanger: ", nrow(d[TARGET == tellerval & KOMMUNE == "Stavanger"])))
  cat(paste0("\nTrondheim: ", nrow(d[TARGET == tellerval & KOMMUNE == "Trondheim"])))

  if(maxrows && nrow(d) > 4000){
    combinations <- length(unique(d$KOMMUNE)) * length(unique(d$TARGET))
    n_obs_per_strata <- floor(8000 / combinations)
    cat(paste0("\nTop ", n_obs_per_strata, " observations shown per MALTALL per KOMMUNE: "))
    d <- d[, .SD[1:n_obs_per_strata], by = c("KOMMUNE", "TARGET")]
  }

  tofactor <-which(!sapply(d, is.numeric))
  d[, (tofactor) := lapply(.SD, as.factor), .SDcols = tofactor]

  return(tab_output(d))
}


