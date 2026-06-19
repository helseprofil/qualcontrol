#' @title readcubes
#' @description
#' Reads cube files for quality control routines
#' @param cube.new Full name of new cube file, including date-tag
#' @param recode.new TRUE/FALSE, recode GEO-codes in new cube?
#' @param cube.old Full name of new cube file, including date-tag
#' @param recode.old TRUE/FALSE, recode GEO-codes in old cube?
#' @param comparecube TRUE/FALSE, should comparecube be generated (default = TRUE)
#' @param outliers Should outliers be detected and flagged?
#' @param dumps save csv-files? Available options are `comparecube`, `newcube_flag`, and `oldcube_flag`
#'
#' @returns New and old file with attributes
#' @export
readfiles <- function(cube.new = NULL,
                      recode.new = FALSE,
                      cube.old = NULL,
                      recode.old = FALSE,
                      comparecube = TRUE,
                      outliers = TRUE,
                      dumps = getOption("qualcontrol.dumps"),
                      useduck = TRUE){
  clean_environment()
  readfiles_checkargs(cube.new, cube.old, recode.new, recode.old, comparecube, outliers)

  cubename <- gsub("^QC_|_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}|\\.csv$|.parquet$", "", cube.new)
  generate_qcfolders(cubename, year = getOption("qualcontrol.year"))
  if(useduck){
    main_db <- find_duckdb_main(cubename)
    init_duckdb_local()
    con <- connect_duckdb_local()
    invisible(DBI::dbExecute(con, sprintf("ATTACH '%s' AS net", main_db)))
    on.exit(DBI::dbExecute(con, "DETACH net"), add = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)

    # NEWCUBE
    exist_newcube <- check_if_table_exist_main(con = con, table = cube.new)
    if(exist_newcube){
      cat("\n- Henter newcube fra database")
      copy_table_from_main_to_local(con = con, table = cube.new, newname = "newcube")
    } else {
      cat("\n- Leser newcube fra scratch og skriver til database")
      load_cubefile_to_duck(con = con, cubefile = cube.new, georecode = recode.new, newname = "newcube")
    }

    # OLDCUBE
    if(!is.null(cube.old)){
      exist_oldcube <- check_if_table_exist_main(con = con, table = cube.old)
      if(exist_oldcube){
        cat("\n- Henter oldcube fra database")
        copy_table_from_main_to_local(con = con, table = cube.old, newname = "oldcube")
      } else {
        cat("\n- Leser oldcube fra scratch og skriver til database")
        load_cubefile_to_duck(con = con, cubefile = cube.old, georecode = recode.old, newname = "oldcube")
      }
    }
  } else {
    newcube <- oldcube <- NULL

    path <- find_cube(cube.new)
    newcube <- read_cube(path)
    newcube <- recode_geo(newcube, recode.new)
    collect_censor_information(dt = newcube)
    newcube <- add_geoparams(newcube)
    assign("newcube", newcube, envir = .GlobalEnv)

    if(!is.null(cube.old)){
      path <- find_cube(cube.old)
      oldcube <- read_cube(path)
      oldcube <- recode_geo(oldcube, recode.old)
      collect_censor_information(dt = oldcube)
      oldcube <- add_geoparams(oldcube)
      assign("oldcube", oldcube, envir = .GlobalEnv)
    }
  }

  if(comparecube) make_comparecube(cube.new = newcube, cube.old = oldcube, outliers = outliers, dumps = dumps)
}

load_cubefile_to_duck <- function(con, cubefile, georecode, newname){
  path <- find_cube(cubefile)
  cube <- read_cube(path)
  cube <- recode_geo(cube, georecode)
  collect_censor_information(dt = cube)
  cube <- add_geoparams(cube)
  write_data_to_main(con = con, table = cubefile, data = cube)
  copy_table_from_main_to_local(con = con, table = cubefile, newname = newname)
}

#' @keywords internal
#' @noRd
readfiles_checkargs <- function(cube.new, cube.old, recode.new, recode.old, comparecube, outliers){
  if (is.null(cube.new) ||
      !grepl(".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}(\\.csv)?$", cube.new))
    stop("cube.new must be provided in the format FILENAME_YYYY-MM-DD-hh-mm")

  if (!is.null(cube.old) &&
      !grepl(".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}(\\.csv)?$", cube.old))
    stop("cube.old must be NULL or provided in the format FILENAME_YYYY-MM-DD-hh-mm")

  if(!is.logical(recode.new))
    stop("recode.new must be TRUE/FALSE")

  if(!is.logical(recode.old))
    stop("recode.old must be TRUE/FALSE")

  if(!is.logical(comparecube))
    stop("comparecube must be TRUE/FALSE")

  if(!is.logical(outliers))
    stop("outliers must be TRUE/FALSE")

  invisible()
}

#' @keywords internal
#' @noRd
find_cube <- function(cubename){

  if(is.null(cubename)) return(NULL)
  path <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.cubefiles"))

  qc_files <- list.files(file.path(path, "QC"), pattern = cubename, full.names = T)
  if(length(qc_files) == 1 && file.exists(qc_files)) return(qc_files)
  if(length(qc_files) > 1){
    parquet_file <- qc_files[grep(".parquet$", qc_files)]
    if(length(parquet_file) == 1 && file.exists(parquet_file)) return(parquet_file)
  }
  if(length(qc_files) == 0){
    qc_files <- list.files(file.path(path, "DATERT/csv"), pattern = cubename, full.names = T)
    if(length(qc_files) == 1 && file.exists(qc_files)) return(qc_files)
  }

  if(length(qc_files) > 1) stop("> 1 file with the same name found:\n", paste0("- ", qc_files, collapse = "\n"))
  if(length(qc_files) == 0) stop(cubename, " not found in QC or DATERT, check spelling")
}

#' @keywords internal
#' @noRd
read_cube <- function(filepath){
  charcols <- getOption("qualcontrol.alldimensions")[!getOption("qualcontrol.alldimensions") %in% c("GEO", "KJONN", "UTDANN", "INNVKAT", "LANDBAK")]
  filetype <- ifelse(grepl(".parquet$", filepath), "PARQUET", "CSV")
  dt <- switch(filetype,
               CSV = do_read_csv(filepath, charcols),
               PARQUET = do_read_parquet(filepath, charcols))

  # dt <- data.table::fread(filepath, encoding = "UTF-8")
  # data.table::setattr(dt, "Filename", basename(filepath))

  .orgnames <- names(data.table::copy(dt))
  data.table::setnames(dt,
                       old = names(getOption("qualcontrol.rename")),
                       new = as.character(getOption("qualcontrol.rename")),
                       skip_absent = T)
  .newnames <- names(dt)

  .diff <- ifelse(any(.orgnames != .newnames), "yes", "no")
  # data.table::setattr(dt, "colnameinfo", list(orgnames = .orgnames, newnames = .newnames, diff = .diff))
  cat(paste0("\ncube loaded: ", sub("(.*PRODUKTER/)", "", filepath), "\n"))
  if(.diff == "yes"){list_renamecols(.orgnames, .newnames, type)}
  if(grepl("ikkegeoprikket_", filepath)){is_valid_outcols(dt)}

  return(dt)
}

set_cubeattributes <- function(dt, type = c("New", "Old"), filename = NULL){
  data.table::setattr(dt, "Filename", filename)
}

do_read_parquet <- function(filepath, charcols){
  file <- arrow::open_dataset(filepath)
  readschema <- arrow::schema(lapply(names(file), function(x){
    if(x %in% charcols){
      arrow::Field$create(name = x, type = arrow::string())
      } else {
        arrow::Field$create(name = x, type = arrow::float64())
      }
    }))
  file <- try(data.table::as.data.table(arrow::open_dataset(filepath, schema = readschema)))
  if("try-error" %in% class(file)) stop("Error when reading file: ", filepath)
  return(file)
}

do_read_csv <- function(filepath, charcols){
  file <- data.table::fread(filepath, encoding = "UTF-8", colClasses = "character")
  numcols <- names(file)[!names(file) %in% charcols]
  file[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
  return(file)
}


#' @keywords internal
#' @noRd
list_renamecols <- function(org, new){
    namechange <- data.table::data.table(org, new)[org != new]
    namechange <- namechange[, let(change = paste(org, "==>", new))][, change]
    cat("\nColumns renamed:", paste("\n *", namechange))
}

#' @keywords internal
#' @noRd
is_valid_outcols <- function(dt){

  outcols <- names(dt)[names(dt) %in% c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER")]
  if(length(outcols) > 0){
    cat("\n\nNB! Filen inneholder ", paste(outcols, collapse = ", "), ". Er dette ok for ALLVIS?", sep = "")
  }
}

#' @keywords internal
#' @noRd
recode_geo <- function(dt, recode){
  if(!recode) return(dt)

  geoyear <- attributes(.georecode)$year
  recodings <- .georecode[old %in% dt$GEO][order(old)]
  if(nrow(recodings) == 0) return(dt)

  cat(paste0("\nRecoding ", nrow(recodings), " geographical codes to ", geoyear, "-codes"))
  dt[, let(origgeo = GEO)]
  dt[recodings, on = setNames("old", "GEO"), GEO := i.current]

  geo99 <- dt[grepl("99$", GEO), .N]
  if(geo99 > 0) cat("\n - ", geo99, " rows recoded to invalid 99-geocodes")

  data.table::setattr(dt, "GEOrecode", list(orgcodes = recodings$old, newcodes = recodings$current))
  return(dt)
}

#' @keywords internal
#' @noRd
#' @description
#' Adds columns GEOniv to identify geographical levels and WEIGHTS to represent population size.
#' Uses population info from "sysdata.rda", stored as the internal object .popinfo
#' Adds the columns by reference, no need to overwrite object.
#' If invalid 99-geocodes are found, they are set manually to FKBV depending on nchar(GEO)
#' @examples
#' # add_geoparams(dt)
add_geoparams <- function(dt){
  dt <- collapse::join(dt, .popinfo, on = "GEO", verbose = 0)
  if(any(is.na(collapse::funique(dt$GEOniv)))){
    dt[is.na(GEOniv), GEOniv := data.table::fcase(nchar(GEO) %in% c(1,2), "F",
                                                  nchar(GEO) %in% c(3,4), "K",
                                                  nchar(GEO) %in% c(5,6), "B",
                                                  nchar(GEO) %in% c(9,10), "V")]
  }
  dt[, GEOniv := droplevels(GEOniv)]
  dt[is.na(WEIGHTS), let(WEIGHTS = 0)]
  return(dt)
}

#' @title collect_censor_information
#' @description
#' If information on secondary censoring is only provided splitted into naboprikketIOmgX-columns,
#' collect them into column naboprikket (0|1). If naboprikket exists, only keep this column. If no
#' secondary censoring column exist, add naboprikket = NA_real_
#' @param dt data
#' @keywords internal
#' @noRd
collect_censor_information <- function(dt){
  for(col in getOption("qualcontrol.prikkeinfo")){
    if(col %in% names(dt)){
      data.table::set(dt, j = col, value = as.integer(dt[[col]]))
    } else {
      data.table::set(dt, j = col, value = NA_integer_)
    }
  }

  naboprikkcols <- grep("^naboprikketIomg", names(dt), value = T)
  if("naboprikket" %in% names(dt)){ # Future standard from khfunctions
    dt[, naboprikket := as.integer(naboprikket)]
    if(length(naboprikkcols) > 0) dt[, (naboprikkcols) := NULL]
    return(invisible(NULL))
  }

  if(length(naboprikkcols) > 0){
    dt[, naboprikket := 0L]
    idx <- which(rowSums(dt[, .SD, .SDcols = naboprikkcols]) > 0)
    data.table::set(dt, i = idx, j = "naboprikket", value = 1L)
    dt[, (naboprikkcols) := NULL]
    return(invisible(NULL))
  }

  dt[, naboprikket := NA_integer_]
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
add_csv <- function(string){
  if(grepl(".*\\.csv$", string)) return(string)
  return(paste0(string, ".csv"))
}

#' @keywords internal
#' @noRd
clean_environment <- function(){
  dbfile <- file.path(fs::path_home(), "helseprofil/duck/qcduck.duckdb")
  if(file.exists(dbfile)){
    try({
      con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
      DBI::dbDisconnect(con_tmp, shutdown = TRUE)
    }, silent = TRUE)
    gc()
    files <- c(dbfile,paste0(dbfile, ".wal"),paste0(dbfile, ".tmp"))
    fs::file_delete(files[fs::file_exists(files)])
  }

  allobjects <- ls(envir = globalenv())
  rmobjects <- grep("newcube|oldcube|newcube_flag|oldcube_flag|comparecube|qcduck", allobjects)
  if(length(rmobjects) > 0)
  rm(list = allobjects[rmobjects], pos = globalenv())
}

