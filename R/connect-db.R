#' @keywords internal
#' @noRd
ConnectKHelsa <- function(){
  path <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.db"))
  if(!file.exists(path)) stop("Finner ikke databasefilen ", path)

  RODBC::odbcDriverConnect(
    paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
      "DBQ=", path, ";"
    )
  )
}

#' @keywords internal
#' @noRd
ConnectGeokoder <- function(){
  path <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.dbgeo"))
  if(!file.exists(path)) stop("Finner ikke databasefilen ", path)

  RODBC::odbcDriverConnect(
    paste0(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
      "DBQ=", path, ";"
    )
  )
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
