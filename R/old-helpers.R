UnspecifiedBydel <- function(data = dfnew,
                             minteller_kommune = NULL,
                             maxrows = TRUE){

  kommunegeo <- c(301, 1103, 4601, 5001)
  bydelsgeo <- unique(data[GEO>9999]$GEO)

  # If no data on bydel, stop and return NULL
  if(length(bydelsgeo) < 1){
    cat("No geo-codes corresponding to bydel, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  }

  d <- data.table::copy(data)

  # Identify dimension and value columns
  .IdentifyColumns(d)

  # Remove years with no data on bydel
  bydelaar <- d[GEO %in% bydelsgeo & SPVFLAGG == 0, unique(AAR)]
  d <- d[AAR %in% bydelaar]

  # If no rows left after filtering years, stop and return NULL.
  if(nrow(d) < 1){
    cat("No rows left in data after removing years without data on bydel.\n")
    return(invisible(NULL))
  }

  # Pick relevant TELLER and NEVNER columns.
  # sumX_uprikk > sumX > X
  tellerval <- .find_compare(data, "TELLER")
  nevnerval <- .find_compare(data, "NEVNER")

  vals <- c(tellerval, nevnerval)

  if(length(vals) < 1){
    cat("No TELLER or NEVNER columns found in data, not possible to estimate unspecified bydel.\n")
    return(invisible(NULL))
  }

  outcols <- c(.dims1, vals, "SPVFLAGG")

  # Create subset, create geolevel and kommune variable
  d <- d[GEO %in% c(kommunegeo, bydelsgeo), ..outcols]
  d[, ':=' (KOMMUNE = character(),
            GEONIV = "Bydel")]
  d[grep("^301", GEO), KOMMUNE := "Oslo"]
  d[grep("^1103", GEO), KOMMUNE := "Stavanger"]
  d[grep("^4601", GEO), KOMMUNE := "Bergen"]
  d[grep("^5001", GEO), KOMMUNE := "Trondheim"]
  d[GEO < 9999, GEONIV := "Kommune"]

  # Identify complete strata within kommune and all dims except GEO
  # count number of missing tellerval and nevnerval in each strata
  d <- data.table::melt(d, measure.vars = c(vals), variable.name = "MALTALL")
  d[, sumprikk := sum(is.na(value)),
    by = c("KOMMUNE",
           stringr::str_subset(c(.dims1, "MALTALL"), "^GEO$", negate = TRUE))]

  # Only keep complete strata
  d <- d[sumprikk == 0]
  if(nrow(d) < 1){
    cat("No complete strata, not possible to estimate unspecified bydel. Was bydelstart set to the correct year?\n")
    return(invisible(NULL))
  }

  # sum value columns for kommune and bydel, and convert to wide format
  d <- d[, .(sum = sum(value, na.rm = T)), by = c("KOMMUNE", "GEONIV", "MALTALL",
                                                  stringr::str_subset(.dims1, "^GEO$", negate = TRUE))]

  d <- data.table::dcast(d, ... ~ GEONIV, value.var = "sum")

  # If minteller provided, exclude rows where MALTALL == tellerval and bydel or kommune < minteller
  if(is.numeric(minteller_kommune)){
    d <- d[!(MALTALL == tellerval & Kommune < minteller_kommune)]
  }

  # Estimate unknown bydel
  d[, `UOPPGITT, %` := 100*(1 - Bydel/Kommune)]

  # Convert all dimensions to factor for search function, round numeric columns
  convert <- stringr::str_subset(names(d), stringr::str_c("^", c(.dims1, "KOMMUNE", "MALTALL"), "$", collapse = "|"))
  d[, (convert) := lapply(.SD, as.factor), .SDcols = convert]
  d[Bydel == 0 & Kommune == 0, `UOPPGITT, %` := NA_real_]
  round <- which(sapply(d, is.numeric))
  d[, (round) := lapply(.SD, round, 2), .SDcols = round]

  # Order according to unspecified bydel
  d <- d[order(-`UOPPGITT, %`)]

  ### Consider saving to environment and make file dump, especially when files are too large for HTML-table

  # Find number of kommune and maltall
  n_kommune <- length(unique(d$KOMMUNE))
  n_maltall <- length(unique(d$MALTALL))

  # Print sumary information
  cat(paste0("Total number of strata with complete bydel (teller + nevner): ", nrow(d)))
  cat(paste0("\nOslo: ", nrow(d[KOMMUNE == "Oslo"])))
  cat(paste0("\nBergen: ", nrow(d[KOMMUNE == "Bergen"])))
  cat(paste0("\nStavanger: ", nrow(d[KOMMUNE == "Stavanger"])))
  cat(paste0("\nTrondheim: ", nrow(d[KOMMUNE == "Trondheim"])))

  # If nrow is > 8 000, show maximum 10 000 observations
  if(maxrows && nrow(d) > 8000){

    # Estimate observations per kommune*maltall to get total <= 8000
    n_obs <- floor(8000 / (n_kommune*n_maltall))

    cat(paste0("\nTop ", n_obs, " observations shown per MALTALL per KOMMUNE: "))

    d <- d[, .SD[1:n_obs], by = c("KOMMUNE", "MALTALL")]
  }

  # Set column order output
  data.table::setcolorder(d,
                          c("KOMMUNE",
                            stringr::str_subset(.dims1, "^GEO$", negate = TRUE),
                            "MALTALL",
                            "Kommune",
                            "Bydel"))

  # Make datatable output (max )
  DT::datatable(d,
                filter = "top",
                rownames = F,
                options = list(
                  columnDefs = list(list(targets = c("Bydel", "Kommune"),
                                         searchable = FALSE)),
                  # Show length menu, table, pagination, and information
                  dom = 'ltpi')
  )

}












.find_compare <- function(data,
                          type){
  if(type == "TELLER"){
    val <- data.table::fcase("sumTELLER_uprikk" %in% names(data), "sumTELLER_uprikk",
                             "sumTELLER" %in% names(data), "sumTELLER",
                             "TELLER_uprikk" %in% names(data), "TELLER_uprikk",
                             "TELLER" %in% names(data), "TELLER",
                             default = NA_character_)
  }

  if(type == "NEVNER"){
    val <- data.table::fcase("sumNEVNER_uprikk" %in% names(data), "sumNEVNER_uprikk",
                             "sumNEVNER" %in% names(data), "sumNEVNER",
                             "NEVNER_uprikk" %in% names(data), "NEVNER_uprikk",
                             "NEVNER" %in% names(data), "NEVNER",
                             default = NA_character_)
  }

  val
}

.IdentifyColumns <- function(data1 = NULL,
                             data2 = NULL){

  if(is.null(data1)){
    stop("data1 not provided in .IdentifyColumns()")
  }

  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }

  .dims1 <<- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
  .vals1 <<- stringr::str_subset(names(data1), stringr::str_c("\\b",.dims1, "\\b", collapse = "|"), negate = T)

  # Create objects relevant for data2
  .dims2 <<- NULL
  .vals2 <<- NULL
  .commondims <<- NULL
  .newdims <<- NULL
  .expdims <<- NULL
  .commonvals <<- NULL
  .newvals <<- NULL
  .expvals <<- NULL
  .commoncols <<- NULL

  # If second data is provided, replace objects above
  if(!is.null(data2)){
    .dims2 <<- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
    .vals2 <<- stringr::str_subset(names(data2), stringr::str_c(.dims2, collapse = "|"), negate = T)
    .commondims <<- .dims1[.dims1 %in% .dims2]
    .newdims <<- stringr::str_subset(.dims1, stringr::str_c("\\b", .dims2, "\\b", collapse = "|"), negate = T)
    .expdims <<- stringr::str_subset(.dims2, stringr::str_c("\\b", .dims1, "\\b", collapse = "|"), negate = T)
    .commonvals <<- .vals1[.vals1 %in% .vals2]
    .newvals <<- stringr::str_subset(.vals1, stringr::str_c("\\b", .vals2, "\\b", collapse = "|"), negate = T)
    .expvals <<- stringr::str_subset(.vals2, stringr::str_c("\\b", .vals1, "\\b", collapse = "|"), negate = T)
    .commoncols <<- c(.commondims, .commonvals)
  }
}
