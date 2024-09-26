#' @title check_friskvik
#' @description
#' Runs a series of checks on the files in the most recent GODKJENT-folder, comparing the file to the corresponding
#' complete data file.
#' @param profile "FHP" or "OVP"
#' @param geolevel "B", "K", or "F"
#' @param profileyear 4-digit year
#' @return csv-file in QualControl/FRISKVIKSJEKK
#' @export
check_friskvik <- function(profile = c("FHP", "OVP"),
                           geolevel = c("B", "K", "F"),
                           profileyear = NULL,
                           test = FALSE){

  if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }

  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }

  if(is.null(profileyear)) profileyear <- getOption("qualcontrol.year")

  # Establish ACCESS connection
  con <- ConnectKHelsa()

  # Generate friskvikpath and kubepath, and list of all datatags in the most recent FRISKVIK/GODKJENT-folder
  friskvikpath <- friskvik_create_path(profile = profile,
                                       geolevel = geolevel,
                                       profileyear = profileyear)
  friskvikfiles <- list.files(friskvikpath, pattern = ".csv")

  # Create savepath and report name
  savepath <- file.path(getOption("qualcontrol.root"),
                        getOption("qualcontrol.output"),
                        "FRISKVIKSJEKK")

  # Add test or profileyear-folder if not existing
  if(test){
    savedir <- file.path(savepath, "testmappe")
  } else {
    savedir <- file.path(savepath, profileyear)
  }

  if(!dir.exists(savedir)){
    dir.create(savedir, recursive = T)
  }

  # Create output file name, with date tag matching GODKJENT folder
  savename <- paste0(file.path(savedir,
                               paste(PROFILE, GEOLEVEL, sub(".*(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})$", "\\1", friskvikpath), sep = "_")),
                     ".csv")

  # Loop trouch friskvikfiles, generate 1-line output per file
  output <- purrr::map_df(friskvikfiles, \(file)  {
    # Try to load files
    tryload <- try(friskvik_read_file(filename = file,
                                      geolevel = geolevel,
                                      profile = profile,
                                      profileyear = profileyear,
                                      friskvikpath = friskvikpath,
                                      con = con),
                   silent = T)

    # Define output columns
    Friskvik_name <- file
    Kube_name <- NA
    FRISKVIK_YEAR <- NA
    Last_year <- NA
    Identical_prikk <- NA
    Matching_kubecol <- NA
    Different_kubecol <- NA
    ETAB <- NA
    KJONN <- NA
    ALDER <- NA
    UTDANN <- NA
    INNVKAT <- NA
    LANDBAK <- NA
    ENHET <- NA
    REFVERDI_VP <- NA
    VALID_COMBINATION <- NA
    NESSTAR <- NA
    Periode_bm <- NA
    Periode_nn <- NA

    # If both files are read without error, replace output columns
    if(!("try-error" %in% class(tryload))){

      Kube_name <- attributes(KUBE)$Filename
      Last_year <- friskvik_last_year()
      Identical_prikk <- friskvik_compare_prikk()

      compvals <- friskvik_compare_value()

      Matching_kubecol <- compvals$matches
      Different_kubecol <- compvals$different

      FRISKVIK_YEAR <- friskvik_unique_level(FRISKVIK, "AAR")
      ETAB <- friskvik_unique_level(FRISKVIK, "ETAB")
      KJONN <- friskvik_unique_level(KUBE, "KJONN")
      ALDER <- friskvik_unique_level(KUBE, "ALDER")
      UTDANN <- friskvik_unique_level(KUBE, "UTDANN")
      INNVKAT <- friskvik_unique_level(KUBE, "INNVKAT")
      LANDBAK <- friskvik_unique_level(KUBE, "LANDBAK")

      # Get info from ACCESS or SPECfile
      friskvikindikator <- stringr::str_extract(Friskvik_name, ".*(?=_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})")
      kubeindikator <- stringr::str_extract(Kube_name, ".*(?=_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})")

      ENHET <- friskvik_read_access(con, "Enhet", "FRISKVIK", friskvikindikator, profile, geolevel, profileyear)
      if(length(ENHET) == 0 | is.na(ENHET)){
        ENHET <- "!!MISSING"
      }

      Periode_bm <- friskvik_read_access(con, "Periode_bm", "FRISKVIK", friskvikindikator, profile, geolevel, profileyear)
      if(length(Periode_bm) == 0 | is.na(Periode_bm)){
        Periode_bm <- "!!empty"
      }

      Periode_nn <- friskvik_read_access(con, "Periode_nn", "FRISKVIK", friskvikindikator, profile, geolevel, profileyear)
      if(length(Periode_nn) == 0 | is.na(Periode_nn)){
        Periode_nn <- "!!empty"
      }

      REFVERDI_VP <- SPEC[Kolonne == "REFVERDI_VP", Innhold]
      if(length(REFVERDI_VP) == 0 | is.na(REFVERDI_VP)){
        REFVERDI_VP <- "!!MISSING from SPECS-file"
      }

      isAK <- data.table::fcase(stringr::str_detect(ENHET, "\\([ak,]+\\)"), TRUE,
                                default = FALSE)

      isPD <- data.table::fcase(REFVERDI_VP %in% c("P", "D"), TRUE,
                                default = FALSE)

      isMEIS <- data.table::fcase("MEIS" %in% Matching_kubecol, TRUE,
                                  default = FALSE)

      VALID_COMBINATION <- data.table::fcase(all(isAK, isPD, isMEIS) | !(any(isAK, isPD, isMEIS)), "Yes",
                                             default =  "!!No!!")

      NESSTAR <- friskvik_is_nesstar(KUBE, profileyear)
    }

    rm(tryload)

    data.table::data.table(
      Friskvik = Friskvik_name,
      Kube = Kube_name,
      File_in_NESSTAR = NESSTAR,
      FRISKVIK_ETAB = ETAB,
      KUBE_KJONN = KJONN,
      KUBE_ALDER = ALDER,
      KUBE_UTDANN = UTDANN,
      KUBE_INNVKAT = INNVKAT,
      KUBE_LANDBAK = LANDBAK,
      `FRISKVIK_YEAR(S)` = FRISKVIK_YEAR,
      Last_year = Last_year,
      Periode_bm = Periode_bm,
      Periode_nn = Periode_nn,
      Identical_prikk = Identical_prikk,
      Matching_kubecol = Matching_kubecol,
      Different_kubecol = Different_kubecol,
      Enhet = ENHET,
      REFVERDI_VP = REFVERDI_VP,
      VALID = VALID_COMBINATION
    )
  }
  )

  cat("\nOutput generated")

  # Write result
  data.table::fwrite(output,
                     file = savename,
                     sep = ";")

  # Close ACCESS connection
  RODBC::odbcClose(con)

  cat(paste("\nOutput written to", savename))
}

#' @title friskvik_read_file
#' @keywords internal
#' @noRd
#' @param filename name of file
#' @param profile One of "FHP" or "OVP"
#' @param geolevel One of "B", "K", or "F"
#' @param profileyear 4-digit profileyear
#' @param friskvikpath can provide full path, defaults to NULL
#' @param kubefile Exact path to kube, starting with "KOMMUNEHELSA/" or "NORGESHELSA/"
friskvik_read_file <- function(filename = NULL,
                               profile = NULL,
                               geolevel = NULL,
                               profileyear = NULL,
                               friskvikpath = NULL,
                               kubefile = NULL,
                               con = NULL){

  # Check arguments
  if(is.null(filename)) {
    stop("file not selected")
  }

  if(is.null(friskvikpath)){
    if(is.null(profile)){
      stop("profile must be provided")
    }
    if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
      stop("profile must be either 'FHP' or 'OVP'")
    }

    if(is.null(geolevel)){
      stop("geolevel must be provided")
    }
    if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
      stop("geolevel must be either 'B', 'K', or 'F'")
    }

    if(is.null(profileyear)){
      stop("profileyear must be provided")
    }
    if(nchar(profileyear) != 4){
      stop("friskvikyear must be a 4 digit number")
    }
  }

  # Create file paths
  if(is.null(friskvikpath)){
    friskvikpath <- friskvik_create_path(profile = profile,
                                        geolevel = geolevel,
                                        profileyear = profileyear)
  }

  basepath <- file.path(getOption("qualcontrol.root"), "KUBER")

  kubepath_kh <- file.path(basepath,
                           "KOMMUNEHELSA",
                           "DATERT",
                           "csv")

  specpath_kh <- file.path(basepath,
                           "KOMMUNEHELSA",
                           "SPECS")

  kubepath_nh <- file.path(basepath,
                           "NORGESHELSA",
                           "DATERT",
                           "csv")

  specpath_nh <- file.path(basepath,
                           "NORGESHELSA",
                           "SPECS")

  # Find and load FRISKVIK file
  friskvikfile <- list.files(friskvikpath,
                             pattern = filename)

  if(length(friskvikfile) < 1){
    stop("FRISKVIK file not found, check arguments (datotag, profile, geolevel, profileyear)")
  } else if(length(friskvikfile) > 1){
    stop("> 1 FRISKVIK files with the same name identified",
         cat(friskvikfile, sep = "\n"))
  } else {
    # Generate file path FRISKVIK
    friskvik <- file.path(friskvikpath, friskvikfile)
  }

  FRISKVIK <- data.table::fread(friskvik)
  data.table::setattr(FRISKVIK, "Filename", basename(friskvik))
  cat(paste0("FRISKVIK loaded: ",
             stringr::str_extract(friskvikpath, "(?<=KUBER/).*"),
             "/",
             basename(friskvik),
             "\n"))

  # Find (if kubefile is not provided) and load KUBE file
  # Search in kubepath_kh, and if not found search kubepath_nh
  if(is.null(kubefile)){

    pattern <- stringr::str_extract(friskvikfile, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")

    kube <- c(list.files(kubepath_kh, pattern = pattern, full.names = T),
              list.files(kubepath_nh, pattern = pattern, full.names = T))
    specfile <- c(list.files(specpath_kh, pattern = pattern, full.names = T),
                  list.files(specpath_nh, pattern = pattern, full.names = T))

  } else {
    kube <- file.path(basepath, kubefile)
    specfile <- NULL
  }

  # If > 1 kube found and con provided, use ACCESS to extract correct kube name
  if (length(kube) > 1 && !is.null(con)) {
    friskvikindikator <-
      stringr::str_extract(basename(friskvik),
                           ".*(?=_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})")
    correctkube <-friskvik_read_access(con, "KUBE_NAVN", "FRISKVIK", friskvikindikator, profile, geolevel, profileyear)
    kube <- grep(correctkube, kube, value = TRUE)
    specfile <- grep(correctkube, specfile, value = TRUE)
  }

  # Check that only one kube is identified, or stop
  if(length(kube) < 1) {
    stop("corresponding KUBE file not found, check arguments")
  } else if(length(kube) > 1){
    stop("> 1 KUBE files with the same name and dato tag identified",
         cat(kube, sep = "\n"))
  }

  KUBE <- data.table::fread(kube)
  data.table::setattr(KUBE, "Filename", basename(kube))
  data.table::setattr(KUBE, "Kubepath", kube)
  cat(paste0("KUBE loaded: ",
             basename(kube),
             "\n"))

  # Identify dimension and value columns
  .IdentifyColumns(FRISKVIK, KUBE)

  # Filter KUBE to match FRISKVIK strata
  ETAB <- FRISKVIK[, unique(ETAB)]
  if(!is.na(ETAB)){
    KUBE <- KUBE[eval(parse(text = ETAB))]
  }

  filtercols <- stringr::str_subset(.commondims, "AAR", negate = TRUE)
  for(i in filtercols){
    KUBE <- KUBE[get(i) %in% FRISKVIK[, unique(get(i))]]
  }

  # Standard dimension filtering, hard coded for 2023 as dimensions not included in FRISKVIK
  friskvikname <- .GetKubename(FRISKVIK)

  if("INNVKAT" %in% .dims2 & !("INNVKAT" %in% .dims1)){
    KUBE <- KUBE[INNVKAT == 0]
  }

  if("LANDBAK" %in% .dims2 & !("LANDBAK" %in% .dims1)){
    if(friskvikname %in% c("Innvand_0_17",
                           "INNVAND_barn")){
      KUBE <- KUBE[LANDBAK == 100]
    } else {
      KUBE <- KUBE[LANDBAK == 0]
    }
  }

  if("UTDANN" %in% .dims2 & !("UTDANN" %in% .dims1)){
    if(friskvikname %in% c("UTDANNING_NH",
                           "UTDANN_HOY")){
      KUBE <- KUBE[UTDANN == 23]
    } else {
      KUBE <- KUBE[UTDANN == 0]
    }
  }

  # Ensure same order
  data.table::setkeyv(KUBE, .commondims)
  data.table::setkeyv(FRISKVIK, .commondims)

  # Load specfile
  SPEC <<- data.table::fread(specfile)
  cat(paste0("SPEC loaded: ",
             basename(specfile),
             "\n"))

  # Save to global env
  KUBE <<- KUBE
  FRISKVIK <<- FRISKVIK
}

# friskvik_compare_year <- function(data1 = FRISKVIK,
#                                   data2 = KUBE){
#
#   kubeyears <- data.table::data.table(KUBE = data2[, sort(unique(AAR), decreasing = TRUE)])
#   kubeyears[, join := KUBE]
#   friskvikyears <- data.table::data.table(FRISKVIK = data1[, sort(unique(AAR), decreasing = TRUE)])
#   friskvikyears[, join := FRISKVIK]
#
#   out <- data.table::merge.data.table(friskvikyears, kubeyears, by = "join", all.y = T)[sort(join, decreasing = TRUE)]
#   out[,join := NULL]
#
#   out[]
# }

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

  out
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
    paste("NO!! Diff for GEO:", stringr::str_c(geodiff, collapse = ", "))
  }
}

#' @title friskvik_compare_val
#' @keywords internal
#' @noRd
#' @param data1 FRISKVIK
#' @param data2 KUBE
friskvik_compare_val <- function(data1 = FRISKVIK,
                               data2 = KUBE){

  # Only include years included in FRISKVIK
  data2 <- data2[AAR %in% data1[, unique(AAR)]]

  # Find value columns in KUBE
  .IdentifyColumns(data1, data2)
  kubevals <- stringr::str_subset(.vals2, "RATE.n|SPVFLAGG", negate = TRUE)
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

  matches <- stringr::str_c(matches, collapse = ", ")
  different <- stringr::str_c(different, collapse = ", ")

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
friskvik_create_path <- function(profile,
                                 geolevel,
                                 profileyear){

  if(!profile %in% c("FHP", "OVP") | length(profile) != 1){
    stop("profile must be either 'FHP' or 'OVP'")
  }

  if(!geolevel %in% c("B", "K", "F") | length(geolevel) != 1){
    stop("geolevel must be either 'B', 'K', or 'F'")
  }

  if(nchar(profileyear) != 4){
    stop("friskvikyear must be a 4 digit number")
  }

  basepath <- file.path(getOption("qualcontrol.root"),
                        "PRODUKTER",
                        "KUBER")

  GEOLEVEL <<- if(geolevel == "B"){
    "BYDEL"
  } else if(geolevel == "F"){
    "FYLKE"
  } else if(geolevel == "K"){
    "KOMM"
  }

  PROFILE <<- if(profile == "FHP"){
    "FRISKVIK"
  } else if(profile == "OVP"){
    "OVP"
  }

  friskvikpath <- file.path(basepath,
                            paste(PROFILE, GEOLEVEL, sep = "_"),
                            profileyear,
                            "GODKJENT")

  godkjentdir <- max(list.dirs(friskvikpath, full.names = F, recursive = F))

  friskvikpath <- file.path(friskvikpath, godkjentdir)

  friskvikpath
}

#' @title friskvik_is_nesstar
#' @keywords internal
#' @noRd
#' @description
#' Checks wether the KUBE corresponding to FRISKVIK exists in NESSTAR-folder
#' @param KUBE kube
#' @param profile profile
friskvik_is_nesstar <- function(file,
                      year){

  nesstar <- data.table::fcase(grepl("KOMMUNEHELSA", attributes(KUBE)$Kubepath), paste0("KOMMUNEHELSA/KH", year, "NESSTAR"),
                               grepl("NORGESHELSA", attributes(KUBE)$Kubepath), paste0("NORGESHELSA/NH", year, "NESSTAR"))

  nesstarpath <- file.path(getOption("qualcontrol.root"),
                           "PRODUKTER/KUBER",
                           nesstar)

  attributes(KUBE)$Filename %in% list.files(nesstarpath, pattern = ".csv")
}

#' @title friskvik_unique_level
#' @keywords internal
#' @noRd
friskvik_unique_level <- function(data,
                                  dim = NULL){

  if(dim %in% names(data)){
    paste(data[, unique(get(dim))], collapse = ", ")
  } else {
    NA
  }
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
