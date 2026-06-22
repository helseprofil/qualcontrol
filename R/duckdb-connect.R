#' @title find_duckdb_disc
#' @description Finner (eller setter opp) duckdb i QC-mappen for hver fil på nettverksdisk, for innlasting av originalfiler. Brukes i readfiles.
#' @family duck
#' @keywords internal
#' @noRd
find_duckdb_disc <- function(cubename){
  dbdir <- file.path(getOption("qualcontrol.root"),
                     getOption("qualcontrol.output"),
                     getOption("qualcontrol.year"),
                     cubename)
  fs::dir_create(dbdir)
  dbpath <- file.path(dbdir, paste0(cubename, ".duckdb"))

  if(!fs::file_exists(dbpath)) {
    cat("\nOppretter ny duckdb på nettverksdisk...")
    con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbpath)
    DBI::dbDisconnect(con_tmp, shutdown = TRUE)
  }
  dbpath
}

#' @title qc_local_db
#' @family duck
#' @keywords internal
#' @noRd
qc_local_db <- function() {
  list(dir = file.path(fs::path_home(), "helseprofil/duck"),
       db = file.path(fs::path_home(), "helseprofil/duck/QCduck.duckdb"))
}

#' @title init_duckdb_local
#' @family duck
#' @description Setter opp fersk lokal duckdb for kvalitetskontroll
#' @keywords internal
#' @noRd
init_duckdb_local <- function(){
  db <- qc_local_db()
  fs::dir_create(db$dir)

  if(file.exists(db$db)){
    try({
      con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = db$db)
      DBI::dbDisconnect(con_tmp, shutdown = TRUE)
    }, silent = TRUE)
    gc()

    files <- c(db$db, paste0(db$db, ".wal"),paste0(db$db, ".tmp"))
    fs::file_delete(files[fs::file_exists(files)])
  }
  invisible(db$db)
}

#' @title connect_duckdb_local
#' @family duck
#' @description connects to helseprofil/duck/QCduck.duckdb, used in all QC-functions to get data
#' @keywords internal
#' @noRd
connect_duckdb_local <- function(){
  invisible(DBI::dbConnect(duckdb::duckdb(), dbdir = qc_local_db()$db))
}
