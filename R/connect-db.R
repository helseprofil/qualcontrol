#' @keywords internal
#' @noRd
ConnectKHelsa <- function(){
  RODBC::odbcConnectAccess2007("O:/Prosjekt/FHP/PRODUKSJON/STYRING/KHELSA.mdb")
}

#' @keywords internal
#' @noRd
ConnectGeokoder <- function(){
  RODBC::odbcConnectAccess2007("O:/Prosjekt/FHP/PRODUKSJON/STYRING/raw-khelse/geo-koder.accdb")
}

#' @keywords internal
#' @noRd
SQLstring <- function(string){
  return(paste0("'", string, "'"))
}

#' @keywords internal
#' @noRd
SQLdate <- function(date){
  return(format(date, "#%Y-%m-%d#"))
}
