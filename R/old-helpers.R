#' TimeSeries
#'
#' Creates time-series plots to visualize outliers.The purpose of these plots are to
#' evaluate whether an outlier is also unreasonable within its own time series.
#' The plots also contain information regarding the underlying numbers (TELLER or sumTELLER), as
#' small absolute changes may have a big impact on RATE in small geographical units.
#'
#' Plots are stored in folders PLOTT/BP and PLOTT/BPc
#'
#' @param data Dataset flagged for outliers, defaults to dfnew_flag
#' @param change Should plots be based on year-to-year changes. Default = FALSE
#' @param profileyear default = PROFILEYEAR
#'
#' @return
#' @export
#'
#' @examples
TimeSeries <- function(data = dfnew_flag,
                       onlynew = TRUE,
                       change = FALSE,
                       profileyear = PROFILEYEAR,
                       data2 = NULL,
                       overwrite = FALSE){

  if(data[, length(unique(AAR))] < 2){
    cat("Whoops! Only one unique AAR in file, time series not possible. No plots generated.")
    return(invisible(NULL))
  }

  if(is.null(attr(data, "outliercol"))){
    cat("Outliercol not detected, does dfnew_flag contain outlier flags?")
    return(invisible(NULL))
  }

  savefolder <- ifelse(change, "TSc", "TS")
  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = savefolder)
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), savefolder = savefolder)

  # Identify target columns, outlier column, and TELLER column. Create savepath
  .IdentifyColumns(data)
  .val <- attributes(data)$outliercol
  .outlier <- "OUTLIER"
  .newoutlier <- "NEW_OUTLIER"
  .teller <- data.table::fcase("TELLER_uprikk" %in% .vals1, "TELLER_uprikk",
                               "TELLER" %in% .vals1, "TELLER",
                               "sumTELLER" %in% .vals1, "sumTELLER",
                               default = NA_character_)

  if(change){
    .val <- paste0("change_", .val)
    .outlier <- paste0("change_", .outlier)
    .newoutlier <- paste0("change_", .newoutlier)
    filenamebase <- paste0(filenamebase, "_y2y_(", format(Sys.time(), "%H%M"), ")")
  } else {
    filenamebase <- paste0(filenamebase, "_(", format(Sys.time(), "%H%M"), ")")
  }

  # If change is requested but not present, return here
  if(change & !.val %in% .vals1){
    cat("\nChange variable not found in data, year-to-year plot not generated")
    return(invisible(NULL))
  }

  # Cannot filter only new outliers if not present
  if(!.newoutlier %in% names(data) & isTRUE(onlynew)){
    onlynew <- FALSE
    cat(paste0("Column ", .newoutlier, " not present, all outliers are included"))
  }

  # Remove rows with missing data on plot value
  data <- data[!is.na(get(.val))]
  bycols <- stringr::str_subset(.dims1, "\\bAAR\\b", negate = T)

  # Find strata containing > 0 outlier, only keep strata with outliers
  data[, n_outlier := sum(get(.outlier), na.rm = T), by = bycols]
  data <- data[n_outlier > 0]

  # Return here if no strata with outliers exist
  if(nrow(data) == 0){
    cat("\nNo strata containing outliers in data, plots not generated")
    return(invisible(NULL))
  }

  # If data on new/prev outlier, reduce data to only strata with new outliers.
  if(onlynew){
    data[, n_new_outlier := sum(get(.newoutlier), na.rm = T), by = bycols]
    data <- data[n_new_outlier > 0]
  }

  # Return here if no strata with new outliers exist
  if(nrow(data) == 0){
    cat("\nNo strata containing new outliers in data, plots not generated")
    return(invisible(NULL))
  }

  outlierdata <- data[get(.outlier) == 1]

  # For lines, only keep strata with >= 2 non-missing rows.
  data[, n_obs := sum(!is.na(get(.val))), by = bycols]
  linedata <- data[n_obs > 1]

  # Add middle-point for labels
  data[, y_middle := 0.5*(max(get(.val), na.rm = T) + min(get(.val), na.rm = T)), by = bycols]

  # Generate filter to save as multiple files with max 25 panels per page
  facets <- stringr::str_subset(bycols, "GEO", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = data, b = facets, s = 25))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets,
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(data, filedims)

  # Generate global plot elements
  plotby <- c("GEO", facets)
  ylab <- ifelse(change, paste0(stringr::str_remove(.val, "change_"), ", (% change)"), .val)
  plotvar <- paste0("Variable plotted: ", ylab)
  caption <- paste0("Tellervariabel: ", .teller, "\nPlots grouped by: ", paste0(plotby, collapse = ", "))

  if(onlynew){
    if(is.null(data2)){
      filenameold <- "not specified"
    } else {
      filenameold <- attributes(data2)$Filename
    }
    plotvar <- paste0(plotvar, ", only new outliers indicated. Old file: ", filenameold)
  }

  # Generate subsets, filenames, and make/save plot.
  cat(paste0("Plots printed to PLOTT/", savefolder))
  for(i in filter){

    # Generate subsets
    d <- data[eval(parse(text = i))]
    ld <- linedata[eval(parse(text = i))]
    od <- outlierdata[eval(parse(text = i))]

    n_pages <- ceiling(nrow(d[, .N, by = plotby])/25)

    if(nrow(d) > 0){
      # Dynamically generate filename, savepath, and varying plot elements
      if(i == "TRUE"){
        name <- "_alle.pdf"
      } else {
        name <- character()
        for(i in filedims){
          name <- paste0(name, "_", unique(d[[i]]))
        }
        name <- paste0(name, ".pdf")
      }
      filename <- paste0(filenamebase, name)
      savepath <- file.path(savebase, filename)

      subtitle <- paste0(plotvar, "\n")
      for(i in filedims){
        subtitle <- paste0(subtitle, i, ": ", unique(d[[i]]), "\n")
      }

      # Make plot
      p <- ggplot(data = d, aes(x = AAR, y = get(.val)))

      if(.newoutlier %in% names(od)){
        od[, label := factor(fcase(get(.newoutlier) == 0, "Previous outlier",
                                   get(.newoutlier) == 1, "New outlier"),
                             levels = c("Previous outlier", "New outlier"))]
        p <- p +
          geom_point(data = od, aes(color = label), size = 5) +
          scale_color_manual(values = c("Previous outlier" = "blue", "New outlier" = "red")) +
          guides(color = guide_legend(title = NULL)) +
          geom_point()
      } else {
        p <- p +
          geom_point(data = od, color = "red", size = 5) +
          geom_point()
      }

      if(nrow(ld) > 0){
        p <- p +
          geom_line(data = ld, aes(y = get(.val), group = 1))
      }

      if(!is.na(.teller)){
        p <- p +
          ggtext::geom_richtext(aes(label = round(get(.teller),0), y = y_middle),
                                hjust = 0.5, angle = 90, alpha = 0.8, size = 8/.pt)
      }

      p <- p +
        ggh4x::force_panelsizes(cols = unit(8, "cm"),
                                rows = unit(5, "cm")) +
        labs(y = ylab,
             caption = caption,
             subtitle = subtitle) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

      # Save plot
      if(file.exists(savepath) & !overwrite){
        cat("\n", basename(savepath), "already exists")
      } else {
        pdf(savepath, width = 18, height = 12)
        for(i in 1:n_pages){
          print(p +
                  ggforce::facet_wrap_paginate(plotby,
                                               labeller = labeller(.multi_line = F),
                                               scales = "free_y",
                                               ncol = 5,
                                               nrow = 5,
                                               page = i))
        }
        dev.off()
        cat(paste0("\n...", filename))
      }
    }
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
