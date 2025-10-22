# Special checks for population and population growth

#' @title check_befvekst
#' @description
#' Loads BEFOLK_GK and BEFVEKST, as identified in the statusfolder in ACCESS.
#' BEFOLK_GK is filtered to contain the same strata as BEFVEKST, and population growth is calculated using lead TELLER values.
#' BEFVEKST TELLER is compared to the calculated population growth from BEFOLK, and all nonmatching rows, if any, are returned
#'
#' @param statusfolder name of statusfolder in access
#' @export
check_befvekst <- function(statusfolder = NULL){
  if(is.null(statusfolder)) stop("name of statusfolder (in ACCESS) must be provided, e.g. 'KUBESTATUS_2026'")
  con <- ConnectKHelsa()
  on.exit(RODBC::odbcCloseAll())
  files <- data.table::setDT(RODBC::sqlQuery(con, paste0("SELECT KUBE_NAVN, DATOTAG_KUBE FROM ", statusfolder, " WHERE KUBE_NAVN IN ('BEFOLK_GK', 'BEFVEKST')")))
  filenames <- files[KUBE_NAVN %in% c("BEFVEKST", "BEFOLK_GK"), filenames := paste0(KUBE_NAVN, "_", DATOTAG_KUBE)][, filenames]
  if(length(filenames) != 2) stop("Antall filer funnet i ", statusfolder, "er ikke lik 2. Trenger både BEFVEKST og BEFOLK_GK for å fortsette")
  cat(paste0("\nFiles identified from table: ", statusfolder, "\n", paste0("- ", filenames, collapse = "\n")))
  befvekst_file <- grep("BEFVEKST", filenames, value = T)
  befolk_file <- grep("BEFOLK_GK", filenames, value = T)
  readfiles(cube.new = befvekst_file, cube.old = befolk_file, recode.new = F, recode.old = F, comparecube = F, outliers = F, dumps = NULL)
  befvekst <- .GlobalEnv$newcube
  befolk <- .GlobalEnv$oldcube
  dims <- identify_coltypes(befvekst, befolk)$commondims
  for(dim in dims){
    befolk <- befolk[get(dim) %in% unique(befvekst[[dim]])]
  }
  data.table::setkeyv(befvekst, dims)
  data.table::setkeyv(befolk, dims)
  calculate_befvekst_for_befolk(befolk, dims = dims)
  data.table::setnames(befvekst, "TELLER", "BEFVEKST_BEFVEKST")

  comp <- collapse::join(befvekst[, .SD, .SDcols = c(dims, "BEFVEKST_BEFVEKST")], befolk[, .SD, .SDcols = c(dims, "BEFVEKST_BEFOLK")], on = dims, how = "l", overid = 2, verbose = 0)
  comp[, diff := BEFVEKST_BEFVEKST - BEFVEKST_BEFOLK]
  comp <- comp[AAR != max(AAR)]

  unique_diff <- collapse::funique(comp$diff)
  if(length(unique_diff) == 1 && unique_diff == 0){
    cat("\nFull match mellom", befvekst_file, "OG", befolk_file)
    return(invisible(NULL))
  } else {
    cat("\n IKKE full match mellom BEFVEKST og BEFOLK_GK, se tabellen for å finne diffene")
    convert_coltype(comp, dims, "factor")
    tab_output(comp, nosearchcolumns = grep("BEFVEKST", names(comp), value = T))
  }
}

calculate_befvekst_for_befolk <- function(dt, dims){
  g <- collapse::GRP(dt, setdiff(dims, "AAR"))
  dt[, leadteller := collapse::flag(collapse::get_vars(dt, "TELLER"), n = -1L, g = g)]
  dt[, let(BEFVEKST_BEFOLK = leadteller - TELLER)][, leadteller := NULL]
}
