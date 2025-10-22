#' @title check_friskvik
#' @description
#' Runs a series of checks on the files in the most recent GODKJENT-folder, comparing the file to the corresponding
#' complete data file.
#' @param profile "FHP" or "OVP"
#' @param geolevel "B", "K", or "F"
#' @param profileyear 4-digit year
#' @param save write CSV?
#' @param test write to testfolder?
#' @return Table in global environment and csv-file in QualControl/FRISKVIKSJEKK if save = TRUE
#' @export
check_friskvik <- function(profile = c("FHP", "OVP"),
                           geolevel = c("B", "K", "F"),
                           profileyear = NULL,
                           save = TRUE,
                           test = FALSE){
  if(!profile %in% c("FHP", "OVP") | length(profile) != 1) stop("profile must be either 'FHP' or 'OVP'")
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1) stop("geolevel must be either 'B', 'K', or 'F'")
  if(is.null(profileyear)) profileyear <- getOption("qualcontrol.year")

  con <- ConnectKHelsa()
  on.exit(RODBC::odbcClose(con), add = TRUE)
  paths <- friskvik_create_path(profile = profile, geolevel = geolevel, profileyear = profileyear, test = test)
  friskvikfiles <- list.files(paths$godkjent, pattern = ".csv")
  outcols <- c("Friskvik", "Kube", "File_in_NESSTAR", "FRISKVIK_ETAB", "KUBE_KJONN", "KUBE_ALDER", "KUBE_UTDANN", "KUBE_INNVKAT", "KUBE_LANDBAK",
               "FRISKVIK_YEAR", "Last_year", "Periode_bm", "Periode_nn", "Identical_prikk", "Matching_kubecol", "Different_kubecol", "Enhet", "REFVERDI_VP", "VALID")
  out_format <- data.table::setDT(as.list(setNames(rep(NA_character_, length(outcols)), outcols)))
  out <- data.table::copy(out_format[0, ])

  for(file in friskvikfiles){
    newline <- out_format
    newline[["Friskvik"]] <- file

    tryload <- try(friskvik_read_file(filename = file, geolevel = geolevel, profile = profile, profileyear = profileyear, friskvikpath = paths$godkjent, con = con),
                   silent = T)
    if(!("try-error" %in% class(tryload))){
      indikatornavn <- sub("(.*)(_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}.*)", "\\1", file)
      newline[["Kube"]] <- attributes(KUBE)$Filename
      newline[["File_in_NESSTAR"]] <- friskvik_in_publication(KUBE, profileyear)
      newline[["FRISKVIK_ETAB"]] <- friskvik_unique_level(FRISKVIK, "ETAB")
      newline[["KUBE_KJONN"]] <- friskvik_unique_level(KUBE, "KJONN")
      newline[["KUBE_ALDER"]] <- friskvik_unique_level(KUBE, "ALDER")
      newline[["KUBE_UTDANN"]] <- friskvik_unique_level(KUBE, "UTDANN")
      newline[["KUBE_INNVKAT"]] <- friskvik_unique_level(KUBE, "INNVKAT")
      newline[["KUBE_LANDBAK"]] <- friskvik_unique_level(KUBE, "LANDBAK")
      newline[["FRISKVIK_YEAR"]] <- friskvik_unique_level(FRISKVIK, "AAR")
      newline[["Last_year"]] <- friskvik_last_year()

      Periode_bm <- friskvik_read_access(con, "Periode_bm", "FRISKVIK", indikatornavn, profile, geolevel, profileyear)
      newline[["Periode_bm"]] <- ifelse(length(Periode_bm) == 0 || is.na(Periode_bm), "!! empty", Periode_bm)
      Periode_nn <- friskvik_read_access(con, "Periode_nn", "FRISKVIK", indikatornavn, profile, geolevel, profileyear)
      newline[["Periode_nn"]] <- ifelse(length(Periode_nn) == 0 || is.na(Periode_nn), "!! empty", Periode_nn)

      newline[["Identical_prikk"]] <- friskvik_compare_prikk()
      compvals <- friskvik_compare_val()
      newline[["Matching_kubecol"]] <- compvals$matches
      newline[["Different_kubecol"]] <- compvals$different

      ENHET <- friskvik_read_access(con, "Enhet", "FRISKVIK", indikatornavn, profile, geolevel, profileyear)
      newline[["Enhet"]] <- ifelse(length(ENHET) == 0 || is.na(ENHET), "!!MISSING", ENHET)
      REFVERDI_VP <- SPEC[Kolonne == "REFVERDI_VP", Innhold]
      newline[["REFVERDI_VP"]] <- ifelse(length(REFVERDI_VP) == 0 || is.na(REFVERDI_VP), "!! MISSING from SPECS-file", REFVERDI_VP)
      isAK <- grepl("\\([ak,]+\\)", newline[["Enhet"]])
      isPD <- newline[["REFVERDI_VP"]] %in% c("P", "D")
      isMEIS <- "MEIS" %in% newline[["Matching_kubecol"]]
      newline[["VALID"]] <- ifelse(all(isAK, isPD, isMEIS) | !(any(isAK, isPD, isMEIS)), "Yes", "!! No!!")
    }
    rm(tryload)
    out <- data.table::rbindlist(list(out, newline))
  }

  assign(paste("FRISKVIKSJEKK",profile,geolevel, sep = "_"), out, envir = .GlobalEnv)

  cat("\nOutput generated")
  if(save){
    data.table::fwrite(out, file = paths$save, sep = ";")
    cat(paste("\nOutput written to", paths$save))
  }
}

#' @title friskvik_read_file
#' @keywords internal
#' @noRd
#' @param filename name of file
#' @param profile One of "FHP" or "OVP"
#' @param geolevel One of "B", "K", or "F"
#' @param profileyear 4-digit profileyear
#' @param friskvikpath can provide full path, defaults to NULL
friskvik_read_file <- function(filename = NULL,
                               profile = NULL,
                               geolevel = NULL,
                               profileyear = NULL,
                               friskvikpath = NULL,
                               con = NULL){
  clean_friskvik_environment()
  if(is.null(filename)) stop("file not selected")
  if(is.null(friskvikpath)){
    if(is.null(profile)) stop("profile must be provided")
    if(!profile %in% c("FHP", "OVP") | length(profile) != 1) stop("profile must be either 'FHP' or 'OVP'")
    if(is.null(geolevel)) stop("geolevel must be provided")
    if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1) stop("geolevel must be either 'B', 'K', or 'F'")
    if(is.null(profileyear)) stop("profileyear must be provided")
    if(nchar(profileyear) != 4) stop("friskvikyear must be a 4 digit number")
  }

  basepath <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.files"))

  # Find and load FRISKVIK file
  friskvikfile <- list.files(friskvikpath, pattern = filename)
  friskvikindikator <- sub("(.*)(_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}.csv$)", "\\1", friskvikfile)
  if(length(friskvikfile) < 1) stop("FRISKVIK file not found, check arguments (datotag, profile, geolevel, profileyear)")
  if(length(friskvikfile) > 1) stop("> 1 FRISKVIK files with the same name identified", cat(friskvikfile, sep = "\n"))
  friskvikfilepath <- file.path(friskvikpath, friskvikfile)
  friskvikdatetag <- sub(".*(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})(.csv$)", "\\1", friskvikfilepath)
  FRISKVIK <<- read_friskvik(path = friskvikfilepath)

  correctcube <- friskvik_read_access(con, "KUBE_NAVN", "FRISKVIK", friskvikindikator, profile, geolevel, profileyear)
  cubefilepath <- get_cube_path(basepath = basepath, datetag = friskvikdatetag, correctcube = correctcube)
  specfilepath <- get_specfile_path(basepath = basepath, datetag = friskvikdatetag, correctcube = correctcube)

  KUBE <- read_friskvik_cube(path = cubefilepath)
  SPEC <<- data.table::fread(specfilepath)
  cat(paste0("SPEC loaded: ", basename(specfilepath), "\n"))

  colinfo <- identify_coltypes(FRISKVIK, KUBE)
  KUBE <- filter_cube_to_friskvik(cube = KUBE, friskvik = FRISKVIK, colinfo = colinfo, friskvikindikator = friskvikindikator)

  data.table::setkeyv(KUBE, colinfo$commondims)
  data.table::setkeyv(FRISKVIK, colinfo$commondims)

  KUBE <<- KUBE
}

clean_friskvik_environment <- function(){
  .GlobalEnv[["FRISKVIK"]] <- NULL
  .GlobalEnv[["KUBE"]] <- NULL
  .GlobalEnv[["SPEC"]] <- NULL
}

read_friskvik <- function(path){
  file <- data.table::fread(path)
  data.table::setattr(file, "Filename", basename(path))
  cat(paste0("FRISKVIK loaded: ", sub("(.*KUBER/)(.*)", "\\2", path), "\n"))
  return(file)
}

read_friskvik_cube <- function(path){
  if(length(path) < 1) stop("corresponding KUBE file not found, check arguments")
  if(length(path) > 1) stop("> 1 KUBE files with the same name and dato tag identified", cat(path, sep = "\n"))

  KUBE <- data.table::fread(path)
  data.table::setattr(KUBE, "Filename", basename(path))
  data.table::setattr(KUBE, "Kubepath", path)
  cat(paste0("KUBE loaded: ", basename(path), "\n"))
  return(KUBE)
}

get_cube_path <- function(basepath, datetag, correctcube){
  kubepath <- file.path(basepath, "STATBANK", "DATERT", "csv")
  path <- c(list.files(kubepath, pattern = datetag, full.names = T))

  if(length(path) > 1) path <- grep(correctcube, path, value = TRUE)
  return(path)
}

get_specfile_path <- function(cubefile, basepath, datetag, correctcube){
  specpath <- file.path(basepath, "STATBANK", "SPECS")

  if(!is.null(cubefile)) datetag <- basename(cubefile)

  path <- c(list.files(specpath, pattern = datetag, full.names = T))

  if(length(path) > 1) path <- grep(correctcube, path, value = TRUE)
  return(path)
}

filter_cube_to_friskvik <- function(cube, friskvik, colinfo, friskvikindikator){
  friskvikindikator <-
  ETAB <- friskvik[, unique(ETAB)]
  if(!is.na(ETAB)) cube <- cube[eval(parse(text = ETAB))]
  filtercols <- grep("^AAR$", colinfo$commondims, invert = T, value = T)
  for(i in filtercols) cube <- cube[get(i) %in% friskvik[, unique(get(i))]]

  if("INNVKAT" %in% colinfo$expdims) cube <- cube[INNVKAT == 0]
  if("LANDBAK" %in% colinfo$expdims){
    landbakval <- ifelse(friskvikindikator %in% c("Innvand_0_17", "INNVAND_barn"), 100, 0)
    cube <- cube[LANDBAK == landbakval]
  }
  if("UTDANN" %in% colinfo$expdims){
    utdannval <- ifelse(friskvikindikator %in% c("UTDANNING_NH","UTDANN_HOY"), 23, 0)
    cube <- cube[UTDANN == utdannval]
  }

  return(cube)
}


#' @title friskvik_last_year
#' @keywords internal
#' @noRd
#' @param data1 FRISKVIK
#' @param data2 KUBE
friskvik_last_year <- function(data1 = FRISKVIK,
                             data2 = KUBE){

  if(length(data1[, unique(AAR)]) > 1){
    lastyear <- max(data1[, unique(AAR)])
    out <- data.table::fcase(lastyear == max(data2[, unique(AAR)]), "Yes",
                             default = "!!NO!!")
  } else if(data1[, unique(AAR)] == max(data2[, unique(AAR)])){
    out <- "Yes"
  } else {
    out <- "!!NO!!"
  }

  return(out)
}

#' @title friskvik_compare_prikk
#' @keywords internal
#' @noRd
#' @param data1 FRISKVIK
#' @param data2 KUBE
friskvik_compare_prikk <- function(data1 = FRISKVIK,
                                   data2 = KUBE){

  # Only include years included in FRISKVIK
  data2 <- data2[AAR %in% data1[, unique(AAR)]]

  # Compare values censored in FRISKVIK with values censored in KUBE
  if(isTRUE(all.equal(is.na(data1$MEIS), data2[, SPVFLAGG > 0]))){
    "Yes"
  } else {
    geodiff <- data1[is.na(data1$MEIS) != data2[, SPVFLAGG > 0], GEO]
    paste("NO!! Diff for GEO:", paste0(geodiff, collapse = ", "))
  }
}

#' @title friskvik_compare_val
#' @keywords internal
#' @noRd
#' @param data1 FRISKVIK
#' @param data2 KUBE
friskvik_compare_val <- function(data1 = FRISKVIK, data2 = KUBE){

  # Only include years included in FRISKVIK
  data2 <- data2[AAR %in% data1[, unique(AAR)]]

  # Find value columns in KUBE
  colinfo <- identify_coltypes(data1, data2)
  kubevals <- grep("RATE.n|SPVFLAGG", colinfo$vals.old, invert = TRUE, value = T)
  # Compare FRISKVIK$MEIS to all value columns to find match
  matches <- character()
  different <- character()

  # Map over value columns in KUBE, find the column(s) matching FRISKVIK$MEIS
  for(i in kubevals){
    if(isTRUE(all.equal(data1$MEIS, data2[, get(i)]))){
      matches <- c(matches, i)
    } else {
      different <- c(different, i)
    }
  }

  if(length(matches) == 0){
    matches <- "!!NO MATCH!!"
  }

  matches <- paste0(matches, collapse = ", ")
  different <- paste0(different, collapse = ", ")

  list(matches = matches, different = different)
}


#' @title friskvik_create_path
#' @keywords internal
#' @noRd
#' @description
#' Helper function to create path to most recent FRISKVIK/GODKJENT-folder
#' @param profile profile
#' @param geolevel geolevel
#' @param profileyear year
friskvik_create_path <- function(profile, geolevel, profileyear, test = FALSE){

  if(!profile %in% c("FHP", "OVP") | length(profile) != 1) stop("profile must be either 'FHP' or 'OVP'")
  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1) stop("geolevel must be either 'B', 'K', or 'F'")
  if(nchar(profileyear) != 4) stop("friskvikyear must be a 4 digit number")

  GEOLEVEL  <- switch(geolevel,
                      "B" = "BYDEL",
                      "F" = "FYLKE",
                      "K" = "KOMM")
  PROFILE <- ifelse(profile == "FHP", "FRISKVIK", profile)

  friskvikpath <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.files"),
                            paste(PROFILE, GEOLEVEL, sep = "_"), profileyear, "GODKJENT")

  godkjentdate <- max(list.dirs(friskvikpath, full.names = F, recursive = F))

  savedir <- file.path(getOption("qualcontrol.root"),getOption("qualcontrol.output"),"FRISKVIKSJEKK")
  savedir <- ifelse(test, file.path(savedir, "testmappe"), file.path(savedir, profileyear))
  if(!dir.exists(savedir)) dir.create(savedir, recursive = T)
  savename <- paste0(file.path(savedir, paste(PROFILE, GEOLEVEL, godkjentdate, sep = "_")), ".csv")

  return(list(godkjent = file.path(friskvikpath, godkjentdate),
              save = savename))
}

#' @title friskvik_in_publication
#' @keywords internal
#' @noRd
#' @description
#' Checks wether the KUBE corresponding to FRISKVIK exists in NESSTAR-folder
#' @param KUBE kube
#' @param profile profile
friskvik_in_publication <- function(file, year){
  pub_folder <- paste0("STATBANK/STATBANK_", year)
  pub_folder <- file.path(getOption("qualcontrol.root"),getOption("qualcontrol.files"), pub_folder)
  attributes(KUBE)$Filename %in% list.files(pub_folder, pattern = ".csv")
}

#' @title friskvik_unique_level
#' @keywords internal
#' @noRd
friskvik_unique_level <- function(data, dim = NULL){
  if(!dim %in% names(data)) return(NA_character_)
  paste(data[, unique(get(dim))], collapse = ", ")
}

#' @title friskvik_read_access
#' @keywords internal
#' @noRd
#' @description Helper function to read single element from KHELSA database
#' @param con connection object created by .ConnectKHelsa
#' @param targetcol specify target column in the selected table
#' @param table specify table name
#' @param name refer to `INDIKATOR`/`KUBE_NAVN` columns
#' @param profile only for FRISKVIK, refer to `PROFILTYPE`` ("FHP", "OVP")
#' @param geolevel only for FRISKVIK, refer to `MODUS` ("B", "K", "F")
#' @param profileyear only for FRISKVIK, refer to `AARGANG`
friskvik_read_access <- function(con,
                                 targetcol,
                                 table,
                                 name,
                                 profile = NULL,
                                 geolevel = NULL,
                                 profileyear = NULL){

  if(table == "FRISKVIK"){
    RODBC::sqlQuery(con,
                    paste0("SELECT ", targetcol,
                           " FROM FRISKVIK ",
                           " WHERE PROFILTYPE='", profile, "'",
                           " AND AARGANG=", profileyear,
                           " AND MODUS='", geolevel, "'",
                           " AND INDIKATOR='", name, "'"),
                    as.is = T)[[1]]
  } else if (table == "KUBER"){
    RODBC::sqlQuery(con,
                    paste0("SELECT ", targetcol,
                           " FROM KUBER ",
                           " WHERE KUBE_NAVN='", name, "'"),
                    as.is = T)[[1]]
  }
}
