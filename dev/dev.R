# DEV

# Read test files

readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
          modus.new = "KH",
          cube.old = "TRANGBODDHET_2023-01-03-14-38",
          modus.old = "KH",
          recode.old = T)

cube.new = data.table::copy(newcube)
cube.old = data.table::copy(oldcube)

DT::datatable(cube.new[1:150],
              filter = "top",
              options = list(
                columnDefs = list(list(targets = c("BODD", "AAR", "GEO"),
                                       searching = FALSE)))
              )


library(microbenchmark)

ComparePrikkTS(newcube, oldcube)
compare_censoring_timeseries(newcube, oldcube)


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
