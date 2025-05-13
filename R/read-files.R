# Temporary file to copy old functions while rewriting

#' @title readcubes
#' @description
#' Reads cube files for quality control routines
#'
#' @param newcubefile new cube filename, in the format "NAME_1234-12-34-56-78"
#' @param modusnew "KH" or "NH"
#' @param recode.newcube TRUE/FALSE, should GEO codes in new file be recoded to current GEO
#' @param oldcubefile old/comparison cube filename, in the format "NAME_1234-12-34-56-78" or NULL
#' @param modusold "KH" or "NH"
#' @param recode.oldcube TRUE/FALSE, should GEO codes in old file be recoded to current GEO
#'
#' @returns New and old file with attributes
#' @export
readfiles <- function(cube.new = NULL,
                      modus.new = c("KH", "NH"),
                      recode.new = FALSE,
                      cube.old = NULL,
                      modus.old = c("KH", "NH"),
                      recode.old = FALSE,
                      comparecube = TRUE,
                      outliers = TRUE,
                      dumps = getOption("qualcontrol.dumps")){
  clean_environment()
  newcube <- oldcube <- NULL

  modus.new <- match.arg(modus.new)
  modus.old <- match.arg(modus.old)
  readfiles_checkargs(cube.new, cube.old, modus.new, modus.old, recode.new, recode.old)

  cube.new <- add_csv(cube.new)
  path.new <- find_cube(cube.new, modus.new)
  newcube <- read_cube(path.new, type = "New")
  newcube <- recode_geo(newcube, recode.new)
  add_geoparams(newcube)

  if(!is.null(cube.old)){
    cube.old <- add_csv(cube.old)
    path.old <- find_cube(cube.old, modus.old)
    oldcube <- read_cube(path.old, type = "Old")
    oldcube <- recode_geo(oldcube, recode.old)
    add_geoparams(oldcube)
  }

  newcube <<- newcube
  oldcube <<- oldcube

  if(comparecube) make_comparecube(cube.new = newcube, cube.old = oldcube, outliers = outliers, dumps = dumps)
}

#' @keywords internal
#' @noRd
readfiles_checkargs <- function(cube.new,
                                cube.old,
                                modus.new,
                                modus.old,
                                recode.new,
                                recode.old) {
  if (is.null(cube.new) ||
      !grepl(".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}(\\.csv)?$", cube.new))
    stop("cube.new must be provided in the format FILENAME_YYYY-MM-DD-hh-mm")

  if (!is.null(cube.old) &&
      !grepl(".*_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}(\\.csv)?$", cube.old))
    stop("cube.old must be NULL or provided in the format FILENAME_YYYY-MM-DD-hh-mm")

  if(is.null(modus.new) || !modus.new %in% c("KH", "NH"))
    stop('modus.new must be "KH" or "NH"')

  if(is.null(modus.old) || !modus.old %in% c("KH", "NH"))
    stop('modus.old must be "KH" or "NH"')

  if(!is.logical(recode.new))
    stop("recode.new must be TRUE/FALSE")

  if(!is.logical(recode.old))
    stop("recode.old must be TRUE/FALSE")

  invisible()
}

#' @keywords internal
#' @noRd
find_cube <- function(cubename,
                      cubemodus = c("KH", "NH"),
                      force_datert = FALSE){

  if(is.null(cubename)) return(NULL)

  cubemodus <- match.arg(cubemodus)
  mode <- switch(cubemodus,
                 "KH" = getOption("qualcontrol.mode")$kh,
                 "NH" = getOption("qualcontrol.mode")$nh
                 )

  path <- file.path(getOption("qualcontrol.root"),
                    getOption("qualcontrol.files"),
                    mode)

  if(!force_datert){
  qc_file <- list.files(file.path(path, "QC"), pattern = cubename, full.names = T)
  if(length(qc_file) == 1 && file.exists(qc_file)) return(qc_file)
  }

  datert_file <- list.files(file.path(path, "DATERT/csv"), pattern = cubename, full.names = T)
  if(length(datert_file) == 1 && file.exists(datert_file)) return(datert_file)

  stop(cubename, " not found, check spelling or modus")
}

#' @keywords internal
#' @noRd
read_cube <- function(filepath, type = c("New", "Old")){

  type <- match.arg(type)

  dt <- data.table::fread(filepath, encoding = "UTF-8")
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
  dt[.popinfo, let(GEOniv = i.GEOniv, WEIGHTS = i.WEIGHTS), on = "GEO"]
  dt[is.na(GEOniv) & grepl("99$", GEO), GEOniv := data.table::fcase(nchar(GEO) == 2, "F",
                                                                    nchar(GEO) == 4, "K",
                                                                    nchar(GEO) == 6, "B",
                                                                    nchar(GEO) == 8, "V")]
  dt[, GEOniv := forcats::fct_drop(GEOniv)]
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

