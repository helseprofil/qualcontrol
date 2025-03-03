#' @title check_nevner_change
#' @description
#' compare NEVNER towards the last and the maximum NEVNER in the time series.
#' Aggregate all dimensions except GEO and AAR
#'
#' @param dt cube file
#' @export
check_nevner_change <- function(dt = newcube,
                                save = TRUE){

  cubefile <- get_cubefilename(dt)
  savepath <- get_table_savefolder(get_cubename(dt))
  suffix <- "nevner_dekningsgrad"

  d <- data.table::copy(dt)[GEOniv %in% c("K", "B")]
  colinfo <- identify_coltypes(d)
  aggdims <- grep("^GEO$|^AAR$", colinfo$dims.new, value = T, invert = T)
  d <- aggregate_cube_multi(d, aggdims)
  nevnercol <- select_nevner_pri(colinfo$vals.new)
  if(is.na(nevnercol)) return("no nevner in file")
  d <- d[!is.na(get(nevnercol))]
  bycols <- grep("AAR", colinfo$dims.new, invert = T, value = T)
  d <- d[, mget(c("AAR", bycols, nevnercol))]

  data.table::setkeyv(d, c(bycols, "AAR"))
  g <- collapse::GRP(d, bycols)

  d[, sumNEVNER_last := collapse::flag(d[[nevnercol]], g = g)]
  d[, sumNEVNER_last := zoo::na.locf(sumNEVNER_last, na.rm = F), by = bycols]
  d[, sumNEVNER_max := collapse::fmax(d[[nevnercol]], g = g, TRA = 1)]
  d[, sumNEVNER_vs_last := round(get(nevnercol)/sumNEVNER_last, 2)]
  d[, sumNEVNER_vs_max := round(get(nevnercol)/sumNEVNER_max, 2)]

  dims <- c("GEO", "AAR")
  if("ALDER" %in% names(d)) dims <- c(dims, "ALDER")

  d <- d[, mget(c(dims, nevnercol, "sumNEVNER_max", "sumNEVNER_last", "sumNEVNER_vs_last", "sumNEVNER_vs_max"))]
  convert_coltype(d, dims, "factor")

  if(save) save_table_output(table = d, savepath = savepath, cubefile = cubefile, suffix = suffix)
  return(tab_output(d[order(sumNEVNER_vs_max)]))
}
