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

#' BoxPlot
#'
#' Creates boxplots to visualize outliers.
#' Plots are stored in folders PLOTT/BP and PLOTT/BPc
#'
#' @param data Dataset flagged for outliers
#' @param onlynew Should only new outliers be indicated on the plot? Default = TRUE
#' @param change Should plots be based on year-to-year changes. Default = FALSE
#' @param profileyear default = PROFILEYEAR
#' @param data2 old file, only used
#' @param overwrite
BoxPlot <- function(data = dfnew_flag,
                    onlynew = TRUE,
                    change = FALSE,
                    profileyear = PROFILEYEAR,
                    data2 = NULL,
                    overwrite = FALSE){

  if(is.null(attr(data, "outliercol"))){
    cat("Outliercol not detected, does dfnew_flag contain outlier flags?")
    return(invisible(NULL))
  }

  # Extract kubename and create path and base filename
  savefolder <- ifelse(change, "BPc", "BP")
  savebase <- .getPlotSaveBase(profileyear = profileyear, kubename = .GetKubename(data), savefolder = savefolder)
  filenamebase <- .getPlotFilenameBase(kubename = .GetKubename(data), datetag = .GetKubedatetag(data), savefolder = savefolder)

  # Remove rows with missing GEOniv (probably 99-codes) and remove unused GEOniv levels.
  data <- data[!is.na(GEOniv)][, GEOniv := base::droplevels(GEOniv)]

  # Identify dimensions, value column, and outliercolumns
  .IdentifyColumns(data)
  .val <- attributes(data)$outliercol
  .outlier <- "OUTLIER"
  .newoutlier <- "NEW_OUTLIER"
  .quantiles <- c("wq25", "wq50", "wq75")
  .ollimits <- c("LOW", "HIGH")

  if(change){
    .val <- paste0("change_", .val)
    .outlier <- paste0("change_", .outlier)
    .newoutlier <- paste0("change_", .newoutlier)
    .quantiles <- paste0("change_", .quantiles)
    .ollimits <- paste0("change_", .ollimits)
  }

  # If change is requested but not present, return here
  if(change & !.val %in% .vals1){
    cat("\nChange variable not found in data, year-to-year plot not generated")
    return(invisible(NULL))
  }

  # Cannot filter only new outliers if not present
  if(onlynew & !.newoutlier %in% names(data)){
    onlynew <- FALSE
    cat(paste0("Column ", .newoutlier, " not present, all outliers are included"))
  }

  # Extract data to generate boxplots, including N observations per strata, and maximum and minimum non-outlier for boxplot whiskers
  bycols <- c("GEOniv", stringr::str_subset(.dims1, "\\bGEO\\b|\\bAAR\\b", negate = T))
  g <- collapse::GRP(data, c(bycols, .quantiles, .ollimits))

  baseplotdata <- collapse::join(g[["groups"]],
                                 data[, .(N_obs = collapse::fsum(!is.na(get(.val))),
                                          MINABOVELOW = collapse::fmin(get(.val)[get(.val) >= get(.ollimits[1])]),
                                          MAXBELOWHIGH = collapse::fmax(get(.val)[get(.val) <= get(.ollimits[2])])),
                                      by = bycols],
                                 overid = 0, verbose = 0)
  baseplotdata[, c(.ollimits) := NULL]

  # Extract data containing outliers and add label. If onlynew = TRUE, only extract new outliers.
  if(onlynew){
    outlierdata <- data[get(.newoutlier) == 1]
  } else{
    outlierdata <- data[get(.outlier) == 1]
  }
  outlierdata[, label := paste0(GEO, "'", stringr::str_sub(AAR, -2L, -1L))]
  outlierdata <- outlierdata[, (.SD), .SDcols = c(bycols, .val, "label")]

  # Generate filter to save as multiple files with max 25 panels per page
  facets <- stringr::str_subset(bycols, "GEOniv", negate = TRUE)
  filedims <- character()
  filedims <- c(filedims, .findPlotSubset(d = baseplotdata, b = facets, s = 25))
  if(length(filedims > 0)){
    facets <- stringr::str_subset(facets,
                                  stringr::str_c("^", filedims, "$", collapse = "|"),
                                  negate = TRUE)
  }
  filter <- .findPlotFilter(baseplotdata, filedims)

  # Generate global plot elements
  plotby <- c("GEOniv", facets)
  plotdims <- .allcombs(baseplotdata, plotby)
  n_strata <- nrow(plotdims[, .N, by = facets])
  n_rows <- base::ceiling(n_strata/5)
  title <- paste0("File: ", attributes(baseplotdata)$Filename, ", Date: ", Sys.Date())
  caption <- paste0("Plots grouped by: ", paste0(facets, collapse = ", "))
  ylab <- ifelse(change, paste0(stringr::str_remove(.val, "change_"), ", (% change)"), .val)
  plotvar <- paste0("Variable plotted: ", ylab)

  if(onlynew){
    if(is.null(data2)){
      filenameold <- "not specified"
    } else {
      filenameold <- attributes(data2)$Filename
    }
    plotvar <- paste0(plotvar, ", only new outliers indicated. Old file: ", filenameold)
  }

  # Generate subsets, filenames, and make/save plot.
  for(i in filter){

    # subset baseplotdata and outlierdata
    bp <- baseplotdata[eval(parse(text = i))][N_obs > 2]
    ol <- outlierdata[eval(parse(text = i))]

    # Dynamically generate filename, savepath, and varying plot elements
    if(i == "TRUE"){
      name <- "_alle.png"
    } else {
      name <- character()
      for(i in filedims){
        name <- paste0(name, "_", unique(bp[[i]]))
      }
      name <- paste0(name, ".png")
    }
    filename <- paste0(filenamebase, name)
    savepath <- file.path(savebase, filename)

    subtitle <- plotvar
    for(i in filedims){
      subtitle <- paste0(subtitle, "\n", i, ": ", unique(bp[[i]]))
    }

    # Generate and save plots.
    p <- ggplot(data = plotdims,
                aes(x = GEOniv)) +
      facet_wrap(facets,
                 labeller = labeller(.multi_line = F),
                 scales = "free_x",
                 ncol = 5) +
      scale_x_discrete(limits = rev(levels(ol$GEOniv)),
                       drop = T) +
      labs(y = ylab,
           x = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +
      coord_flip() +
      geom_text(data = ol,
                aes(y = get(.val),
                    label = label),
                angle = 90,
                size = 8/.pt) +
      geom_boxplot(
        data = bp,
        aes(ymin = MINABOVELOW,
            lower = get(.quantiles[1]),
            middle = get(.quantiles[2]),
            upper = get(.quantiles[3]),
            ymax = MAXBELOWHIGH),
        stat = "identity") +
      ggh4x::force_panelsizes(cols = unit(8, "cm"),
                              rows = unit(6, "cm")) +
      theme(text = element_text(color = "black"),
            plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 16),
            plot.caption = element_text(size = 16),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))

    .saveBoxPlot(file = savepath,
                 plot = p,
                 rows = n_rows,
                 overwrite = overwrite)
  }
}

.saveBoxPlot <- function(file,
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
           width = 45,
           height = rows*6 + 10,
           units = "cm")
    cat("\nSave file: ", basename(file))
  }
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
