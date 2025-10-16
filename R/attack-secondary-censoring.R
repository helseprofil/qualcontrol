attack_naboprikk <- function(cubefile = NULL, cubepath = NULL){
  if(is.null(cubefile) & is.null(cubepath)) stop("Du må angi cubefile (med datotag) eller cubepath (full sti) for å bruke spesifikk fil")
  path <- ifelse(is.null(cubepath), find_cube(cubefile), cubepath)
  d <- read_cube(path)
  d <- add_geoparams(d)
  dims <- identify_coltypes(d)$dims.new
  specs <- find_specs(path, geolevels = unique(d$GEOniv))
  naboprikkdims <- grep("nabopr", names(specs), value = T)
  naboprikkdims <- setNames(naboprikkdims, toupper(gsub("Stata_nabopr([^_]+).*$", "\\1", naboprikkdims)))

  out_pvern1 <- data.table::copy(d[0, ..dims])

  for(dim in names(naboprikkdims)){
    restdims <- setdiff(dims, dim)
    triangles <- decode_triangles(dim = dim, naboprikkdims = naboprikkdims, specs = specs)

    for(i in 1:length(triangles)){
      d_subset <- d[get(dim) %in% triangles[[i]]]
      pvern1 <- d_subset[, .(n_pvern = sum(pvern, na.rm = T), n_prikkok = sum(prikket_ok, na.rm = T)), by = restdims][, (dim) := paste0("{", paste(triangles[[i]], collapse = ","), "}")][n_pvern > 0 & n_prikkok == 1][, let(n_pvern = NULL, n_prikkok = NULL)]
      if(ncol(pvern1) > 0) out_pvern1 <- data.table::rbindlist(list(out_pvern1, pvern1), use.names = T)
    }

  }

}

decode_triangles <- function(dim, naboprikkdims, specs){
  triangles <- specs[[naboprikkdims[[dim]]]]
  triangles <- regmatches(triangles, gregexpr("\\{[^}]*\\}", triangles))[[1]]
  triangles <- strsplit(gsub("^\\{|\\}$", "", triangles), ",")
  return(triangles)
}

find_specs <- function(path, geolevels){
  cubefile <- gsub("^QC_|.csv$|.parquet$", "", basename(path))
  specpath <- file.path(getOption("qualcontrol.root"), getOption("qualcontrol.files"), "STATBANK/SPECS")
  specfile <- list.files(specpath, pattern = cubefile, full.names = T)
  if(length(specfile) == 1){
    cat("\n* Henter specs fra specs-fil")
    specs <- data.table::fread(specfile)
    specs <- specs[Tabell == "KUBER" & grepl("^PRIKK_|^Stata_PRIKK|^Stata_nabopr", Kolonne), .SD, .SDcols = c("Kolonne", "Innhold")]
    specs <- data.table::dcast(specs, .~Kolonne, value.var = "Innhold")[, "." := NULL]
  } else {
    cat("\n* Henter specs fra ACCESS")
    cubename <- gsub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}|\\.csv$|.parquet$", "", cubefile)
    con <- ConnectKHelsa()
    on.exit(RODBC::odbcCloseAll())
    specs <- data.table::setDT(RODBC::sqlQuery(con, paste0("SELECT * FROM KUBER WHERE KUBE_NAVN=", SQLstring(cubename)), as.is = TRUE))
    specs <- specs[, .SD, .SDcols = grep("^PRIKK_|^Stata_PRIKK|^Stata_nabopr", names(specs), value = T)]
  }
  naboprikkcols <- grep("nabopr", names(specs), value = T)
  naboprikkcols_empty <- naboprikkcols[is.na(specs[, .SD, .SDcols = naboprikkcols]) | specs[, .SD, .SDcols = naboprikkcols] == ""]
  specs[, names(.SD) := NULL, .SDcols = naboprikkcols_empty]

  add_geotriangles(specs = specs, geolevels = geolevels)
  return(specs)
}

add_geotriangles <- function(specs, geolevels){
  geotriangles <- khfunctions:::get_geonaboprikk_triangles()
  cols <- character()
  for(col in names(geotriangles)){
    levels <- strsplit(sub(".*_([LFKBV]{2})", "\\1", col), "")[[1]]
    if(all(levels %in% geolevels)) cols <- c(cols, col)
  }
  geotriangles_filtered <- geotriangles[, .(Stata_naboprGEO = do.call(paste0, .SD)), .SDcols = cols]
  specs[, names(geotriangles_filtered) := geotriangles_filtered]
}
