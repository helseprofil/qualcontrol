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
