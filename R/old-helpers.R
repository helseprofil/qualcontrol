#' TimelineBydel
#'
#' Plots timelines for each bydel, with the weighted mean timeline for the bydel and kommune superimposed.
#' Used to check the validity of data on bydel level. The mean timeline for bydel should approximately match
#' the timeline for kommune.
#'
#' @param data
#' @param profileyear
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
TimelineBydel <- function(data = dfnew_flag,
                          profileyear = PROFILEYEAR,
                          overwrite = FALSE){

  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = "TL")
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), "TL")

  .IdentifyColumns(data)
  .val <- data.table::fcase("MEIS" %in% .vals1, "MEIS",
                            "RATE" %in% .vals1, "RATE",
                            "SMR" %in% .vals1, "SMR",
                            "MALTALL" %in% .vals1, "MALTALL",
                            default = NA)
  data <- data[!is.na(get(.val))]
  data[, KOMMUNE := data.table::fcase(grepl("^301", GEO), "Oslo",
                                      grepl("^1103", GEO), "Stavanger",
                                      grepl("^4601", GEO), "Bergen",
                                      grepl("^5001", GEO), "Trondheim",
                                      default = "nonrelevant")]
  data <- data[KOMMUNE != "nonrelevant"]

  bycols <- c("KOMMUNE", stringr::str_subset(.dims1, "\\bGEO\\b", negate = T))

  # Generate filter to save as multiple files with max 4 (x 4) panels per page
  facets <- stringr::str_subset(bycols, "\\bKOMMUNE\\b|\\bAAR\\b", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = data, b = facets, s = 5))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets,
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(data, filedims)
  # Add rows for faceting in plot
  data[, rows := interaction(mget(facets))]

  # Split data into bydel/kommune
  d <- data[GEO > 9999]
  d[, n_geo := .N, by = c("GEO", filedims, "rows")]
  kd <- data[GEO %in% c(301, 1103, 4601, 5001) & AAR %in% unique(d$AAR)]

  # estimate weighted mean .val for kommune and bydel
  kg <- collapse::GRP(kd, c(bycols, "rows"))
  kw <- kd$WEIGHTS
  kd <- collapse::fmutate(kg[["groups"]],
                          y = fmean(kd[[.val]], w = kw, g = kg))
  kd[, type := "Kommune"]

  # estimate weighted mean .val for bydel
  bg <- collapse::GRP(d, c(bycols, "rows"))
  bw <- d$WEIGHTS
  bd <- collapse::fmutate(bg[["groups"]],
                          y = fmean(d[[.val]], w = bw, g = bg))
  bd[, type := "Vektet bydel"]

  # Combine trend-data, and remove trends with <= 1 observation
  trends <- data.table::rbindlist(list(kd, bd))
  trends[, N := .N, by = c("KOMMUNE", filedims, "rows", "type")]
  trends <- trends[N > 1]

  # Generate global plot elements
  caption <- paste0("Plots grouped by: ", stringr::str_c(facets, collapse = ", "))
  ylab <- .val
  title <- paste0("File: ", attributes(data)$Filename, ", Date: ", Sys.Date())
  plotvar <- paste0("Variable plotted: ", ylab)
  plotdims <- .allcombs(d, c("KOMMUNE", "rows"))
  n_rows <- nrow(plotdims[, .N, by = rows])

  # Generate subsets, filenames, and make/save plot.
  for(i in filter){

    pd <- d[eval(parse(text = i))]
    td <- trends[eval(parse(text = i))]

    if(nrow(pd) > 0){
      # Dynamically generate filename, savepath, and varying plot elements
      if(i == "TRUE"){
        name <- "_alle.png"
      } else {
        name <- character()
        for(i in filedims){
          name <- paste0(name, "_", unique(pd[[i]]))
        }
        name <- paste0(name, ".png")
      }
      filename <- paste0(filenamebase, name)
      savepath <- file.path(savebase, filename)

      subtitle <- plotvar
      for(i in filedims){
        subtitle <- paste0(subtitle, "\n", i, ": ", unique(pd[[i]]))
      }

      # Generate plot
      p <- ggplot(plotdims) +
        facet_grid(cols = vars(KOMMUNE),
                   rows = vars(rows),
                   switch = "y",
                   scales = "free") +
        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             y = ylab) +
        geom_line(data = td,
                  aes(x = AAR, y = y, color = type, group = type),
                  linewidth = 1.5) +
        scale_color_manual(values = c("red", "blue")) +
        geom_line(data = pd,
                  aes(x = AAR, y = get(.val), group = GEO), linetype = 2) +
        geom_point(data = pd,
                   aes(x = AAR, y = get(.val)),
                   size = 3, shape = 1) +
        guides(color = guide_legend(title = NULL)) +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 90, vjust = 0.5))

      # Save plot
      .saveTimeLine(file = savepath,
                    plot = p,
                    rows = n_rows,
                    overwrite = overwrite)
    }
  }
}

.saveTimeLine <- function(file,
                          plot,
                          rows = n_rows,
                          overwrite = FALSE){
  if(file.exists(file) & !overwrite){
    cat("\n", basename(file), "already exists")
  } else {
    ggsave(filename = file,
           plot = plot,
           device = "png",
           dpi = 300,
           width = 37,
           height = rows*6 + 10,
           units = "cm")
    cat("\nSave file: ", basename(file))
  }
}



#' Find subset of plots
#'
#' The algorithm select the combination of dimensions yielding the maximum number of panels per page, but less than s
#'
#' @param d dataset
#' @param b all bycols
#' @param s maximum number of panels per page
.findPlotSubset <- function(d,
                            b,
                            s){

  orgstrata <- nrow(d[, .N, by = b])

  if(orgstrata <= s){
    return(NULL)
  }

  # Create a reference table containing dim and n levels (this may replace CompareDims(), and called here)
  ref <- data.table(dim = character(), n = numeric())
  for(i in b){
    l <- length(unique(d[[i]]))
    ref = data.table::rbindlist(list(ref,
                                     data.table(dim = i,
                                                n = l)))
  }

  # Generate a table with all combinations of dimensions
  combs <- data.table()
  for(i in seq_along(b)){
    x <- data.table(base::t(utils::combn(b, i)))
    colnames(x) <- paste0("dim", 1:i)
    combs <- data.table::rbindlist(list(combs, x), fill = TRUE)
  }

  # Add columsn showing n levels for each dimension
  for(j in seq_along(b)){
    new <- paste0("n", j)
    old <- paste0("dim", j)
    combs[ref, (new) := i.n, on = setNames("dim", old)]
  }

  # Replace NA with 1 and calculate n files per combination
  data.table::setnafill(combs, fill = 1, cols = grep("^n", names(combs)))
  combs[, files := Reduce("*", .SD), .SDcols = patterns("^n")]

  # Calculate panels per page
  incdims <- combs[, do.call(paste, c(.SD, sep = "|")), .SDcols = patterns("^dim")]
  nondims <- list()
  for(i in seq_along(incdims)){
    nondims[[i]] <- str_subset(b, incdims[i], negate = T)
  }
  nondims <- lapply(nondims, function(x) ref[dim %in% x, n])
  nondims <- as.integer(lapply(nondims, function(x) if(length(x) > 0) Reduce("*", x) else 1))
  combs[, panels := nondims]

  # Select optimal combination
  optimal <- combs[panels <= s][panels == max(panels)]
  if(nrow(optimal > 1)){
    optimal <- optimal[1]
  }
  v <- unlist(optimal[, .SD, .SDcols = patterns("^dim")], use.names = F)
  v <- v[!is.na(v)]
  v
}

#' .findPlotFilter
#'
#' Helper function to filter subset for plotting to different files
#'
.findPlotFilter <- function(data,
                            dims){
  if(length(dims) == 0){
    filter <- "TRUE"
  } else {
    subsets <- GRP(data, dims)[["groups"]]
    cols <- names(subsets)
    for(i in cols){
      subsets[, (i) := paste0(i, "=='", get(i), "'")]
    }
    filter <- subsets[, filter := do.call(paste, c(.SD, sep = " & ")), .SDcols = cols][, (filter)]
  }

  filter
}

.find_compare <- function(data,
                          type){
  if(type == "TELLER"){
    val <- data.table::fcase("sumTELLER_uprikk" %in% names(data), "sumTELLER_uprikk",
                             "sumTELLER" %in% names(data), "sumTELLER",
                             "TELLER_uprikk" %in% names(data), "TELLER_uprikk",
                             "TELLER" %in% names(data), "TELLER",
                             default = NA_character_)
  }

  if(type == "NEVNER"){
    val <- data.table::fcase("sumNEVNER_uprikk" %in% names(data), "sumNEVNER_uprikk",
                             "sumNEVNER" %in% names(data), "sumNEVNER",
                             "NEVNER_uprikk" %in% names(data), "NEVNER_uprikk",
                             "NEVNER" %in% names(data), "NEVNER",
                             default = NA_character_)
  }

  val
}

.IdentifyColumns <- function(data1 = NULL,
                             data2 = NULL){

  if(is.null(data1)){
    stop("data1 not provided in .IdentifyColumns()")
  }

  if(!exists(".ALL_DIMENSIONS")) {
    source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
    .ALL_DIMENSIONS <- ALL_DIMENSIONS
  }

  .dims1 <<- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
  .vals1 <<- stringr::str_subset(names(data1), stringr::str_c("\\b",.dims1, "\\b", collapse = "|"), negate = T)

  # Create objects relevant for data2
  .dims2 <<- NULL
  .vals2 <<- NULL
  .commondims <<- NULL
  .newdims <<- NULL
  .expdims <<- NULL
  .commonvals <<- NULL
  .newvals <<- NULL
  .expvals <<- NULL
  .commoncols <<- NULL

  # If second data is provided, replace objects above
  if(!is.null(data2)){
    .dims2 <<- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
    .vals2 <<- stringr::str_subset(names(data2), stringr::str_c(.dims2, collapse = "|"), negate = T)
    .commondims <<- .dims1[.dims1 %in% .dims2]
    .newdims <<- stringr::str_subset(.dims1, stringr::str_c("\\b", .dims2, "\\b", collapse = "|"), negate = T)
    .expdims <<- stringr::str_subset(.dims2, stringr::str_c("\\b", .dims1, "\\b", collapse = "|"), negate = T)
    .commonvals <<- .vals1[.vals1 %in% .vals2]
    .newvals <<- stringr::str_subset(.vals1, stringr::str_c("\\b", .vals2, "\\b", collapse = "|"), negate = T)
    .expvals <<- stringr::str_subset(.vals2, stringr::str_c("\\b", .vals1, "\\b", collapse = "|"), negate = T)
    .commoncols <<- c(.commondims, .commonvals)
  }
}
