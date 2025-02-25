#' @title check_nevner_change_ungdata
#' @description
#' compare NEVNER towards the last and the maximum NEVNER in the time series.
#'
#' @param dt cube file
#' @export
check_nevner_change_ungdata <- function(dt = newcube){

  cubefile <- get_cubefilename(dt)
  savepath <- get_table_savefolder(get_cubename(dt))
  suffix <- "ungdata_dekningsgrad"

  d <- data.table::copy(dt)[GEOniv %in% c("K", "B")]
  colinfo <- identify_coltypes(d)
  for(dim in c("KJONN", "SOES")){
    if(dim %in% colinfo$dims.new) d <- aggregate_cube(d, dim)
  }
  nevnercol <- select_nevner_pri(colinfo$vals.new)
  if(is.na(nevnercol)) return("no nevner in file")
  d <- d[!is.na(get(nevnercol))]
  bycols <- grep("AAR", colinfo$dims.new, invert = T, value = T)
  d <- d[, mget(c("AAR", bycols, nevnercol))]

  data.table::setkeyv(d, c(bycols, "AAR"))
  g <- collapse::GRP(d, bycols)

  d[, nevnerlag := collapse::flag(d[[nevnercol]], g = g)]
  d[, nevnerlag := zoo::na.locf(nevnerlag, na.rm = F), by = bycols]
  d[, sumNEVNER_max := collapse::fmax(d[[nevnercol]], g = g, TRA = 1)]
  d[, sumNEVNER_vs_last := round(get(nevnercol)/nevnerlag, 2)]
  d[, sumNEVNER_vs_max := round(get(nevnercol)/sumNEVNER_max, 2)]

  d <- d[, mget(c("GEO", "AAR", nevnercol, "sumNEVNER_max", "sumNEVNER_vs_last", "sumNEVNER_vs_max"))]
  convert_coltype(d, c("GEO", "AAR"), "factor")

  save_table_output(table = d, savepath = savepath, cubefile = cubefile, suffix = suffix)
  return(tab_output(d[order(sumNEVNER_vs_max)]))
}
