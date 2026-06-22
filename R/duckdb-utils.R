#' @title get_duckdb_table
#' @description
#' Fetches table from the local database into memory
#' @param tablename name of table to read
#' @returns data.table
#' @export
get_duckdb_table <- function(tablename){
  con <- connect_duckdb_local()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)
  if(!tablename %in% DBI::dbListTables(con)) stop("Finner ikke ", tablename, " i databasen. Har du lastet inn filen med readfiles?")
  d <- DBI::dbReadTable(con, tablename)
  data.table::setDT(d)
  d
}

#' @title overwrite_duckdb_table
#' @description
#' Overwrites a table in the local duckdb. e.g. To be used if you have recoded av variable
#' @param d data
#' @param tablename table name
#' @export
overwrite_duckdb_table <- function(d, tablename){
  con <- connect_duckdb_local()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)
  cat("\n Overskriver", tablename, "i den lokale databasen")
  invisible(DBI::dbWriteTable(con, name = tablename, value = d, overwrite = TRUE))
}
