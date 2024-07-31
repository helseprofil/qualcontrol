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
  con_geo <- ConnectGeokoder()
  tab <- data.table::rbindlist(list(data.table::setDT(RODBC::sqlQuery(con_geo, paste0("SELECT oldCode, currentCode FROM kommune", year))),
                                    data.table::setDT(RODBC::sqlQuery(con_geo, paste0("SELECT oldCode, currentCode FROM fylke", year)))))
  data.table::setnames(tab, c("old", "current"))
  tab <- tab[!is.na(old) & old != current]
  data.table::setattr(tab, "year", year)

  path <- file.path(system.file("data", package = "qualcontrol"), "georecode.rds")

  if(!file.exists(path)) file.create(path)
  if(overwrite) saveRDS(tab, path)

  RODBC::odbcClose(con_geo)
}
