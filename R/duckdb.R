find_duckdb_main <- function(cubename){
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

check_if_table_exist_main <- function(con, table){
  exist <- suppressMessages(DBI::dbExistsTable(DBI::Id(schema = "net", table = table), conn = con))
  return(exist)
}

write_data_to_main <- function(con, table, data){
  invisible(
    DBI::dbWriteTable(con,
                    name = DBI::Id(schema = "net", table = table),
                    value = data,
                    overwrite = TRUE)
  )
}

copy_table_from_main_to_local <- function(con, table, newname){
  invisible(
    DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM net.%s",
                                DBI::dbQuoteIdentifier(con, newname),
                                DBI::dbQuoteIdentifier(con, table)))
  )
}

qc_local_db <- function() {
  list(dir = file.path(fs::path_home(), "helseprofil/duck"),
       db = file.path(fs::path_home(), "helseprofil/duck/QCduck.duckdb"))
}

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

connect_duckdb_local <- function(){
  invisible(DBI::dbConnect(duckdb::duckdb(), dbdir = qc_local_db()$db))
}
