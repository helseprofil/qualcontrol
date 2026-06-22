#' @title get_duckdb_table
#' @description
#' Henter en tabell fra den lokale duckdb-databasen inn i minnet
#' Nyttig dersom du trenger å se en tabell, eller kode om en variabel
#' (f.eks. for å matche oldcube mot newcube), før overskriving med `overwrite_duckdb_table`
#' @param tablename navn på tabellen du vil hente
#' @family duck
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
#' Overskriver en tabell i den lokale duckdb-databasen.
#' Nyttig om du har omkodet en variabel for å matche oldcube mot newcube.
#' @param tablename navn på tabellen du vil hente
#' @family duck
#' @export
overwrite_duckdb_table <- function(d, tablename){
  con <- connect_duckdb_local()
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)
  cat("\n Overskriver", tablename, "i den lokale databasen")
  invisible(DBI::dbWriteTable(con, name = tablename, value = d, overwrite = TRUE))
}
