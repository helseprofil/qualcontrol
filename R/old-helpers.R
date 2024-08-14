PrintTimeseries <- function(dims = .TSplotdims,
                            plots = TS){

  for(i in 1:length(dims)){

    header <-  paste0("\n\n## Across ", dims[i], "\n")
    cat(header)

    print(plots[[i]])

    cat("\n")
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
