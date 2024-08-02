#' @title update_georecode
#' @description
#' Update the georecode table used for recoding geographical codes to current codes for comparison.
#' Connects to the geo-code database and use information for kommune, fylke.
#'
#' @family family name
#'
#' @param year the year for valid geo codes
#' @param overwrite should the current table be replaced by the new?
#' @examples
#' # qualcontrol:::update_georecode(2024, overwrite = T)
update_georecode <- function(year, overwrite = T){
  con <- ConnectGeokoder()
  on.exit(RODBC::odbcClose(con), add = T)
  tab <- data.table::rbindlist(list(data.table::setDT(RODBC::sqlQuery(con, paste0("SELECT oldCode, currentCode FROM kommune", year))),
                                    data.table::setDT(RODBC::sqlQuery(con, paste0("SELECT oldCode, currentCode FROM fylke", year)))))
  data.table::setnames(tab, c("old", "current"))
  tab <- tab[!is.na(old) & old != current]
  data.table::setattr(tab, "year", year)

  path <- file.path(system.file("data", package = "qualcontrol"), "georecode.rds")
  if(!file.exists(path)) file.create(path)
  if(overwrite) saveRDS(tab, path)

  cat("georecode updated, remember to push the new changes to GitHub")
}


#' @title update_popinfo
#' @description
#' Generates popinfo.rds file containing weights and GEOniv including small/large kommune and Helseregion
#'
#' @param popfile complete file name of BEFOLK_GK file
#' @param year referring to NESSTAR-folder
#' @param overwrite should the file be overwritten?
#' @examples
update_popinfo <- function(popfile, overwrite = T){

  tab <- data.table::fread(popfile)
  tab <- tab[KJONN == 0 & ALDER == "0_120" & AAR == max(AAR), .(GEO, TELLER)]
  data.table::setnames(tab, "TELLER", "WEIGHTS")
  tab[, GEOniv := data.table::fcase(GEO == 0, "L",
                                    GEO <= 99, "F",
                                    GEO < 10000 & WEIGHTS >= 10000, "K",
                                    GEO < 10000 & WEIGHTS < 10000, "k",
                                    GEO >= 10000, "B")]
  hreg <- data.table::data.table(GEO = 81:84,
                                 WEIGHTS = 0,
                                 GEOniv = "H")
  tab <- data.table::rbindlist(list(tab, hreg))
  tab[, GEOniv := factor(GEOniv, levels = c("H", "L", "F", "K", "k", "B"))]

  path <- file.path(system.file("data", package = "qualcontrol"), "popinfo.rds")
  if(!file.exists(path)) file.create(path)
  if(overwrite) saveRDS(tab, path)

  cat("popinfo updated, remember to push the new changes to GitHub")
}

#' @keywords internal
#' @noRd
update_dimlist <- function(){
  con <- ConnectKHelsa()
  on.exit(RODBC::odbcClose(con), add = T)
  date <- format(Sys.time(), "#%Y-%m-%d#")
  standarddimensions <- c("GEO", "AAR", "ALDER", "KJONN", "UTDANN", "INNVKAT", "LANDBAK")
  tabdimensions <- data.table::setDT(RODBC::sqlQuery(con, paste0("SELECT TAB1, TAB2, TAB3 FROM FILGRUPPER WHERE VERSJONFRA <=",date, "AND VERSJONTIL >", date)))
  tabdimensions <- data.table::melt(tabdimensions, measure.vars = c("TAB1", "TAB2", "TAB3"))[!is.na(value), unique(value)]
  out <- sort(c(standarddimensions, tabdimensions))
  return(out)
}

#' Update internal data
#' Stores objects in R/sysdata.rda which is needed in the package
#' @return
#' - dimlist
#' @export
update_internal_data <- function(){

  .validdims <- update_dimlist()

  usethis::use_data(.validdims,
                    internal = T,overwrite = T)
}
