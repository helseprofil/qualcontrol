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
                      dumps = getOption("qualcontrol.dumps")){
  clean_environment()
  newcube <- oldcube <- NULL
  readfiles_checkargs(cube.new, cube.old, recode.new, recode.old, comparecube, outliers)

  path.new <- find_cube(cube.new)
  newcube <- read_cube(path.new, type = "New")
  newcube <- recode_geo(newcube, recode.new)
  newcube <- add_geoparams(newcube)

  if(!is.null(cube.old)){
    path.old <- find_cube(cube.old)
    oldcube <- read_cube(path.old, type = "Old")
    oldcube <- recode_geo(oldcube, recode.old)
    oldcube <- add_geoparams(oldcube)
  }

  newcube <<- newcube
  oldcube <<- oldcube

  if(comparecube) make_comparecube(cube.new = newcube, cube.old = oldcube, outliers = outliers, dumps = dumps)
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
read_cube <- function(filepath, type = c("New", "Old")){
  type <- match.arg(type)
  charcols <- getOption("qualcontrol.alldimensions")[!getOption("qualcontrol.alldimensions") %in% c("GEO", "KJONN", "UTDANN", "INNVKAT", "LANDBAK")]
  filetype <- ifelse(grepl(".parquet$", filepath), "PARQUET", "CSV")
  dt <- switch(filetype,
               CSV = do_read_csv(filepath, charcols),
               PARQUET = do_read_parquet(filepath, charcols))

  # dt <- data.table::fread(filepath, encoding = "UTF-8")
  data.table::setattr(dt, "Filename", basename(filepath))
  data.table::setattr(dt, "Filetype", data.table::fcase(grepl("/QC/", filepath), "QC",
                                                        grepl("/DATERT/csv/", filepath), "ALLVIS",
                                                        default = "Other"))
  data.table::setattr(dt, "Cubeversion", type)

  .orgnames <- names(data.table::copy(dt))
  data.table::setnames(dt,
                       old = names(getOption("qualcontrol.rename")),
                       new = as.character(getOption("qualcontrol.rename")),
                       skip_absent = T)
  .newnames <- names(dt)

  .diff <- ifelse(any(.orgnames != .newnames), "yes", "no")
  data.table::setattr(dt, "colnameinfo", list(orgnames = .orgnames, newnames = .newnames, diff = .diff))
  cat(paste0("\n", type, " cube loaded: ", sub("(.*PRODUKTER/)", "", filepath), "\n"))
  if(.diff == "yes"){list_renamecols(.orgnames, .newnames, type)}
  if(type == "New" && grepl("ikkegeoprikket_", attributes(dt)$Filename)){is_valid_outcols(dt)}

  return(dt)
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
list_renamecols <- function(org, new, type){
    namechange <- data.table::data.table(org, new)[org != new]
    namechange <- namechange[, let(change = paste(org, "==>", new))][, change]
    cat("\n", type, "cube columns renamed:", paste("\n *", namechange))
}

#' @keywords internal
#' @noRd
is_valid_outcols <- function(dt){

  outcols <- names(dt)[names(dt) %in% c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER")]
  if(length(outcols) > 0){
    cat("\n\nNB! New file contains ", paste(outcols, collapse = ", "), ". Is this ok for ALLVIS?", sep = "")
  }
}

#' @keywords internal
#' @noRd
recode_geo <- function(dt, recode){
  if(!recode) return(dt)

  geoyear <- attributes(.georecode)$year
  cubeversion <- attributes(dt)$Cubeversion
  recodings <- .georecode[old %in% dt$GEO][order(old)]
  if(nrow(recodings) == 0) return(dt)

  cat(paste0("\nIn ", cubeversion, " cube: Recoding ", nrow(recodings), " geographical codes to ", geoyear, "-codes"))
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

#' @keywords internal
#' @noRd
add_csv <- function(string){
  if(grepl(".*\\.csv$", string)) return(string)
  return(paste0(string, ".csv"))
}

#' @keywords internal
#' @noRd
clean_environment <- function(){
  allobjects <- ls(envir = globalenv())
  rmobjects <- grep("newcube|oldcube|newcube_flag|oldcube_flag|comparecube", allobjects)
  rm(list = allobjects[rmobjects], pos = globalenv())
}

