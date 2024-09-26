# Originally written by Yusman Kamaleri

#' @title check_barometer
#' @author Yusman Kamaleri
#' @description
#' Checks whether identical values have received a colored point marker in the barometer.
#'#'
#' @param type "FHP" or "OVP"
#' @param year production year
#' @param geo one of "fylke", "kommune", "bydel"
#' @param indikator file path indicator
#' @param barometer file path barometer
#' @export
check_barometer <- function(type = c("FHP", "OVP"),
                            year = NULL,
                            geo = c("fylke", "kommune", "bydel"),
                            indikator = NULL,
                            barometer = NULL){

  type <- match.arg(type)
  geo <- tolower(geo)
  geo <- match.arg(geo)
  if(is.null(year)) stop("Year must be provided")

  base <- file.path(getOption("qualcontrol.root"),
                    "PRODUKTER/SSRS_filer")

  path <- file.path(base, type, year, geo, "FigurOgTabell")

  if (is.null(barometer)){
    barometer = "inndataBarometer.dta"
  }

  if (is.null(indikator)){
    indikator <- ifelse(geo == "fylke", "Indikator_F.dta", "Indikator_ny.dta")
  }

  message("Leser filer fra ", path)
  cat("In progress ...")

  ## Barometer --------------------------------
  bar <- haven::read_dta(file.path(path, barometer))

  data.table::setDT(bar)
  barV1 <- c("stedskode_string",
             "stedskode_numeric",
             "indikator_kodet",
             "LPnr",
             "roede",
             "groenne",
             "hvitMprikk")
  bar[, setdiff(names(bar), barV1) := NULL]

  cat("...")
  data.table::setkey(bar, indikator_kodet)

  # Add LPnr if not present (not present i fylke?)
  if(!"LPnr" %in% names(bar)){
    bar[, LPnr := rev(indikator_kodet - 1)]
    data.table::setcolorder(bar, "LPnr", after = "stedskode_numeric")
  }

  if(0 %in% bar$LPnr){
    bar[, corrected_LPnr := LPnr + 1]
    data.table::setcolorder(bar, "corrected_LPnr", after = "LPnr")
    message("OBS: LPnr contain 0, corrected_LPnr corresponds to barometer")
  }

  data.table::setkey(bar, LPnr)
  bar[, indikator_kodet := NULL]

  ## barometer id starts from 2 and is inverse of indikator id
  ## this makes it equivalent to indikator id
  ## bar[, kode := rev(indikator_kodet - 1)]

  ## Indicator --------------------------------
  cat("...")
  indraw <- haven::read_dta(file.path(path, indikator))

  data.table::setDT(indraw)
  indV1 <- grep("Verdi", names(indraw), value = TRUE)
  indVar <- c("Aar", "LPnr", "corrected_LPnr", "Sted_kode", "SpraakId", indV1)
  indraw[, setdiff(names(indraw), indVar) := NULL]
  ind <- indraw[SpraakId == "BOKMAAL"]

  cat("... \n")
  withVar <- c("Aar", indV1)
  bar[ind, (withVar) := mget(withVar), on = c(stedskode_string = "Sted_kode", LPnr = "LPnr")]

  verdiCol <- ifelse(geo == "fylke", "Verdi_mellomGeonivaa", "Verdi_lavesteGeonivaa")
  outDT <- bar[get(verdiCol) == Verdi_referansenivaa, ][
    !is.na(roede) | !is.na(groenne) | !is.na(hvitMprikk)]

  return(outDT)
}
