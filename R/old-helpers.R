#' #' FlagNew
#' #'
#' #' Initiate flagged version of dfnew (dfnew_flag), saved to global environment
#' #' Identifies common and new dimensions compared to old KUBE
#' #' For common dimensions, all new rows are flagged (sets newrow = 1)
#' #' For new dimensions, all rows != 0 (total numbers) are flagged as new
#' #'
#' #'
#' #' @param data1 new KUBE, defaults to dfnew
#' #' @param data2 old KUBE, defaults to dfold
#' #' @param commondims common dimensions for dfnew and old, for flagging of new rows
#' #' @param newdims new dimensions, for flagging of new rows (not totals)
#' #' @param vals value columns in dfnew, for outlier detection
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .FlagNew <- function(data1,
#'                      data2,
#'                      commondims,
#'                      newdims,
#'                      dims,
#'                      vals,
#'                      outlier = TRUE){
#'
#'   # Initiate flagged version of new KUBE (sets newrow = 0), saves to global env
#'   dfnew_flag <<- data.table::copy(data1)[, `:=` (newrow = 0L)]
#'
#'   if(is.null(data2)){
#'     dfnew_flag[, newrow := 1]
#'     cat("\n- No old file, all rows flagged as new")
#'   } else {
#'     # For common dimensions, flag all rows with new levels
#'     # Loops over common dimensions. Flags previously unflagged rows for new levels
#'     purrr::walk(commondims,
#'                 \(x){
#'                   dfnew_flag[!get(x) %in% data2[, unique(get(x))] & newrow == 0,
#'                              newrow := 1L]
#'                 })
#'
#'     cat("\n- For common dimensions, flagged all rows with new levels as new rows")
#'
#'     # For new dimensions, flag any rows != 0 (i.e. not total numbers)
#'     if(length(newdims) != 0) {
#'       purrr::walk(newdims,
#'                   \(x){
#'                     dfnew_flag[get(x) != 0 & newrow == 0,
#'                                newrow := 1L]
#'                   })
#'       cat(paste0("\n- For new dimensions, all rows not = 0 flagged as new rows: ", stringr::str_c(newdims, collapse = ", ")))
#'     }
#'
#'   }
#'
#'   # Flag outliers
#'   if(outlier){
#'     dfnew_flag <<- .FlagOutlier(data = dfnew_flag,
#'                                 dims = dims,
#'                                 vals = vals)
#'   } else {
#'     cat("\n\n- Outliers not flagged in dfnew_flag, as argument outlier = FALSE")
#'   }
#'
#'   dfnew_flag[]
#'
#'   cat("\n- Flagged version of new KUBE created: dfnew_flag\n")
#' }
#'
#' #' FlagOld
#' #'
#' #' Initiate flagged version of dfold (dfold_flag), saved to global environment
#' #' Identifies common and expired dimensions compared to new KUBE
#' #' For common dimensions, all expired rows are flagged (sets exprow = 1)
#' #' For expired dimensions, all rows != 0 (total numbers) are flagged as expired
#' #'
#' #' @param data1 new KUBE, defaults to dfnew
#' #' @param data2 old KUBE, defaults to dfold
#' #' @param dims character vector of dimensions columns, defaults to DIMENSIONS set in INPUT
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .FlagOld <- function(data1,
#'                      data2,
#'                      commondims,
#'                      expdims,
#'                      dims,
#'                      vals,
#'                      outlier = TRUE){
#'
#'   # Initiate flagged version of old KUBE (sets newrow = 0), saves to global env
#'   dfold_flag <<- data.table::copy(data2)[, exprow := 0L]
#'
#'   # For common dimensions, flag all rows with expired levels
#'   # Loops over common dimensions. Flags previously unflagged rows for expired levels
#'   purrr::walk(commondims,
#'               \(x){
#'                 dfold_flag[!get(x) %in% data1[, unique(get(x))] & exprow == 0,
#'                            exprow := 1L]
#'               })
#'
#'   cat("\n- For common dimensions, flagged all rows with expired levels")
#'
#'   # For expired dimensions, flag any rows != 0 (i.e. not total numbers)
#'   if(length(expdims) != 0) {
#'     purrr::walk(expdims,
#'                 \(x){
#'                   dfold_flag[get(x) != 0 & exprow == 0,
#'                              exprow := 1L]
#'                 })
#'     cat("\n- For expired dimensions, flagged all rows not representing total numbers")
#'   }
#'
#'   # Flag outliers
#'   if(outlier){
#'     dfold_flag <<- .FlagOutlier(data = dfold_flag,
#'                                 dims = dims,
#'                                 vals = vals)
#'   } else {
#'     cat("\n\n- Outliers not flagged in dfold_flag, as argument outlier = FALSE")
#'   }
#'
#'   dfold_flag[]
#'
#'   cat("\n- Flagged version of old KUBE created: dfold_flag\n")
#' }
#'
#' #' FixDecimals
#' #'
#' #' Helper function to round value columns according to type
#' #'
#' #' @param data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .FixDecimals <- function(data,
#'                          commonvals){
#'
#'   reldiffcols <- stringr::str_subset(names(data), "_reldiff")
#'   valcols <- stringr::str_subset(names(data), stringr::str_c("^", commonvals, "$", collapse = "|")) |>
#'     stringr::str_subset("_reldiff", negate = TRUE)
#'
#'   round0 <- stringr::str_subset(valcols, "^RATE.n_new$|^RATE.n_old$|SPVFLAGG")
#'   round1 <- stringr::str_subset(valcols, "TELLER|NEVNER")
#'   round2 <- c(stringr::str_subset(valcols, "^RATE_|SMR|MEIS"), reldiffcols)
#'
#'   .myround <- function(data, val, round){
#'     set(data, j = val, value = round(data[[val]], round))
#'   }
#'
#'   purrr::walk(round0, \(x) .myround(data, x, 0))
#'   purrr::walk(round1, \(x) .myround(data, x, 1))
#'   purrr::walk(round2, \(x) .myround(data, x, 2))
#'
#'   data[]
#' }
#'
#' #' CreateCompare
#' #'
#' #' Create combined KUBE for comparison of value columns.
#' #'
#' #' -Removes new or expired rows
#' #' -Merges on common dimensions
#' #' -Select common value columns, with suffixes _new and _old
#' #'
#' #' @param data1 new flagged KUBE
#' #' @param data2 old flagged KUBE
#' #' @param commondims common dimensions
#' #' @param commonvals common values
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .CreateCompare <- function(data1,
#'                            data2,
#'                            commondims,
#'                            commonvals){
#'
#'   # Format new KUBE
#'   cat("\n- Formats new KUBE")
#'   cat("\n  - Remove new rows, select common dimensions and values")
#'   comparenew <- data.table::copy(data1)
#'
#'   # Add TELLER and NEVNER, set equal to TELLER/NEVNER_uprikk if not present in dfnew and present in dfold
#'   # For rows where SPVFLAGG != 0, the generated column is set to NA_real_
#'   # Add the value to commonvals to generate _new/_old/_diff/_reldiff columns
#'   for(i in c("TELLER", "NEVNER", "sumTELLER", "sumNEVNER", "RATE.n")){
#'     if(i %in% names(data2) & !(i %in% names(comparenew)) & paste0(i, "_uprikk") %in% names(comparenew)){
#'       comparenew[, (i) := get(paste0(i, "_uprikk"))]
#'       comparenew[SPVFLAGG != 0, (i) := NA_real_]
#'       commonvals <- c(commonvals, i)
#'     }
#'   }
#'
#'   # Select common dimensions and value columns
#'   comparenew <- comparenew[, mget(c(commondims, commonvals, "newrow"))]
#'   # Add suffix to value columns
#'   commonvals_new <- paste0(commonvals, "_new")
#'   data.table::setnames(comparenew, commonvals, commonvals_new)
#'
#'   # Format old KUBE
#'   cat("\n- Formats old KUBE")
#'   cat("\n  - Remove expired rows, select common dimensions and values")
#'   compareold <- data.table::copy(data2)
#'   # Remove expired rows, select common columns and values
#'   compareold <- compareold[exprow == 0, c(..commondims, ..commonvals)]
#'   # Add suffix to value columns
#'   commonvals_old <- paste0(commonvals, "_old")
#'   data.table::setnames(compareold, commonvals, commonvals_old)
#'
#'   # Create comparedata
#'   compareKUBE <- collapse::join(comparenew, compareold, on = commondims, how = "left", verbose = 0, overid = 0)
#'
#'   colorder <- c(commondims, "newrow")
#'   for(i in commonvals){
#'     colorder <- c(colorder, paste0(i, c("_new", "_old")))
#'   }
#'   data.table::setcolorder(compareKUBE, colorder)
#'
#'   # Create diff columns
#'
#'   for(i in commonvals){
#'
#'     new <- paste0(i, "_new")
#'     old <- paste0(i, "_old")
#'     diff <- paste0(i, "_diff")
#'     reldiff <- paste0(i, "_reldiff")
#'
#'     # Initiate _diff (new - old) and _reldiff (new/old) columns
#'     compareKUBE[, (diff) := get(new) - get(old)]
#'     compareKUBE[, (reldiff) := get(new) / get(old)]
#'     # For rows with missing new or old values, set _diff and _reldiff to NA
#'     compareKUBE[is.na(compareKUBE[[new]]) & !is.na(compareKUBE[[old]]), (diff) := NA_real_]
#'     compareKUBE[is.na(compareKUBE[[new]]) & !is.na(compareKUBE[[old]]), (reldiff) := NA_real_]
#'     # For rows with missing old AND new, set _diff = 0, and _reldiff = 1
#'     compareKUBE[is.na(compareKUBE[[new]]) & is.na(compareKUBE[[old]]), (diff) := 0]
#'     compareKUBE[is.na(compareKUBE[[new]]) & is.na(compareKUBE[[old]]), (reldiff) := 1]
#'   }
#'
#'   # Remove SPVFLAGG_reldiff
#'   compareKUBE[, SPVFLAGG_reldiff := NULL]
#'
#'   compareKUBE <- .FixDecimals(compareKUBE, commonvals)
#'   # Export compareKUBE to global environment
#'   # compareKUBE <<- .FixDecimals(compareKUBE)
#'   compareKUBE <<- compareKUBE
#' }
#'
#' #' Format data
#' #'
#' #' Flags new rows in new KUBE, exports flagged KUBE
#' #'
#' #' Flags expired rows in old KUBE, exports flagged KUBE
#' #'
#' #' Create a combined CompareKUBE for comparison of value columns, merged on common
#' #' dimensions. All common value columns are given suffixes _new, and _old.
#' #'
#' #' The two flagged KUBEs and the CompareKUBE can be saved as .csv-files through
#' #' the dumps
#' #'
#' #'
#' #' @param data1 New KUBE, defaults to dfnew
#' #' @param data2 Old KUBE, defaults to dfold
#' #' @param dumps List of dump points, defaults to .DUMPS (NULL)
#' #' @param profileyear To save output in the correct folder
#' #' @param dfnew_flag_name optional name of filedumps, defaults to NA
#' #' @param dfold_flag_name optional name of filedumps, defaults to NA
#' #' @param overwrite
#' #' @param compareKUBE_name optional name of filedumps, defaults to NA
#' #' @param outlier Flag outliers or not, defaults to TRUE
#' FormatData <- function(data1 = dfnew,
#'                        data2 = dfold,
#'                        dumps = DUMPS,
#'                        profileyear = PROFILEYEAR,
#'                        dfnew_flag_name = NA,
#'                        dfold_flag_name = NA,
#'                        compareKUBE_name = NA,
#'                        overwrite = FALSE,
#'                        outlier = TRUE){
#'
#'   # Create folder structure, if not existing, and set file path for file dumps
#'   kubename <- .GetKubename(data1)
#'
#'   if(!is.null(dumps)){
#'     .CreateFolders(profileyear = profileyear,
#'                    kubename = kubename)
#'   }
#'
#'   dumppath <- file.path("O:/Prosjekt/FHP",
#'                         "PRODUKSJON",
#'                         "VALIDERING",
#'                         "NESSTAR_KUBER",
#'                         profileyear,
#'                         "KVALITETSKONTROLL",
#'                         kubename,
#'                         "FILDUMPER",
#'                         "/")
#'
#'   # Identify dimension and value columns
#'   if(is.null(data2)){
#'     .IdentifyColumns(data1)
#'   } else {
#'     .IdentifyColumns(data1, data2)
#'   }
#'
#'   # Summary of dimensions and values, if data2 provided
#'   if(!is.null(data2)){
#'     msg_commondims <- dplyr::case_when(length(.commondims) == 0 ~ "\n- No common dimensions found",
#'                                        TRUE ~ paste0("\n- Common dimensions found: ", stringr::str_c(.commondims, collapse = ", ")))
#'
#'     msg_newdims <- dplyr::case_when(length(.newdims) == 0 ~ "\n- No new dimensions.",
#'                                     TRUE ~ paste0("\n- New dimensions found: ", stringr::str_c(.newdims, collapse = ", ")))
#'
#'     msg_expdims <- dplyr::case_when(length(.expdims) == 0 ~ "\n- No expired dimensions.",
#'                                     TRUE ~ paste0("\n- Expired dimensions found: ", stringr::str_c(.expdims, collapse = ", ")))
#'
#'     msg_commonvals <- dplyr::case_when(length(.commonvals) == 0 ~ "\n- No common value columns found",
#'                                        TRUE ~ paste0("\n- Common value columns found: ", stringr::str_c(.commonvals, collapse = ", ")))
#'
#'     msg_newvals <- dplyr::case_when(length(.newvals) == 0 ~ "\n- No new value columns.",
#'                                     TRUE ~ paste0("\n- New value columns found: ", stringr::str_c(.newvals, collapse = ", ")))
#'
#'     msg_expvals <- dplyr::case_when(length(.expvals) == 0 ~ "\n- No expired value columns.",
#'                                     TRUE ~ paste0("\n- Expired value columns found: ", stringr::str_c(.expvals, collapse = ", ")))
#'   }
#'
#'   # Flag new KUBE (create dfnew_flag)
#'   cat("STARTS flagging new kube:")
#'   if(!is.null(data2)){
#'     cat(msg_commondims)
#'     cat(msg_newdims)
#'     cat(msg_commonvals)
#'     cat(msg_newvals)
#'   }
#'   .FlagNew(data1 = data1,
#'            data2 = data2,
#'            commondims = .commondims,
#'            newdims = .newdims,
#'            dims = .dims1,
#'            vals = .vals1,
#'            outlier = outlier)
#'
#'   if(!is.null(data2)){
#'     # Flag old KUBE (if data2 provided, create dfold_flag)
#'     cat("\nSTARTS flagging old kube:")
#'     cat(msg_commondims)
#'     cat(msg_expdims)
#'     cat(msg_commonvals)
#'     cat(msg_expvals)
#'     .FlagOld(data1 = data1,
#'              data2 = data2,
#'              commondims = .commondims,
#'              expdims = .expdims,
#'              dims = .dims2,
#'              vals = .vals2,
#'              outlier = outlier)
#'
#'     if(outlier){
#'       # Add PREV_OUTLIER and NEW_OUTLIER to dfnew_flag
#'       cat("\n- Adding PREV_OUTLIER to dfnew_flag\n")
#'
#'       dfnew_flag <<- .AddPrevOutlier(data1 = dfnew_flag,
#'                                      data2 = dfold_flag,
#'                                      commondims = .commondims)
#'
#'       cat("\nCOMPLETED flagging and outlier detection!\n")
#'     } else {
#'       cat("\nCOMPLETED flagging. Ouliers not detected as argument outlier = FALSE\n")
#'     }
#'
#'     cat("\nSTARTS create compareKUBE:")
#'
#'     .CreateCompare(data1 = dfnew_flag,
#'                    data2 = dfold_flag,
#'                    commondims = .commondims,
#'                    commonvals = .commonvals)
#'
#'     cat("\n\n-COMPLETED creating compareKUBE\n")
#'   } else {
#'     dfold_flag <<- NULL
#'     compareKUBE <<- NULL
#'     cat("\n\n-No old KUBE to be flagged, compareKUBE not created\n")
#'   }
#'
#'   # File dumps
#'
#'   datetagnew <- .GetKubedatetag(data1)
#'   if(!is.null(data2)){
#'     datetagold <- .GetKubedatetag(data2)
#'   } else {
#'     datetagold <- ""
#'   }
#'
#'   ## Create list of required dumps and savenames
#'   reqdumpfiles <- data.table::data.table(dumpfiles = c("dfnew_flag", "dfold_flag", "compareKUBE"),
#'                                          savenames = c(dfnew_flag_name, dfold_flag_name, compareKUBE_name))
#'   reqdumpfiles <- reqdumpfiles[dumpfiles %in% dumps]
#'
#'   ## Make sure only valid dumps are required
#'   if("dfold_flag" %in% dumps & is.null(data2)){
#'     cat("FILEDUMP dfold_flag required, but dfold not provided. No filedump written.")
#'     reqdumpfiles <- reqdumpfiles[dumpfiles != "dfold_flag"]
#'   }
#'   if("compareKUBE" %in% dumps & is.null(data2)){
#'     cat("FILEDUMP compareKUBE required, but dfold not provided. No filedump written.")
#'     reqdumpfiles <- reqdumpfiles[dumpfiles != "compareKUBE"]
#'   }
#'
#'   purrr::walk2(reqdumpfiles$dumpfiles,
#'                reqdumpfiles$savenames,
#'                \(x,y) {
#'                  .SaveFiledump(filedump = x,
#'                                savename = y,
#'                                dumppath = dumppath,
#'                                datetagnew = datetagnew,
#'                                datetagold = datetagold,
#'                                overwrite = overwrite)
#'                })
#'
#'   cat("\n\nDONE!")
#'
#' }
#'
#' #' .SaveFiledump
#' #'
#' #' @param filedump "dfnew_flag", "dfold_flag", or "compareKUBE"
#' #' @param savename dfnew_flag_name, dfold_flag_name, or compareKUBE_name
#' #' @param dumps vector of required filedumps
#' #' @param dumppath where to write files
#' #' @param kubename name of kube
#' #' @param datetagnew
#' #' @param datetagold
#' #' @param outdata
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .SaveFiledump <- function(filedump,
#'                           savename,
#'                           dumppath,
#'                           datetagnew,
#'                           datetagold,
#'                           overwrite){
#'
#'   if(filedump == "dfnew_flag"){
#'     outdata <- copy(dfnew_flag)
#'     kubename <- .GetKubename(dfnew_flag)
#'     datetag <- datetagnew
#'     type <- "(new)_FLAGGED.csv"
#'   } else if(filedump == "dfold_flag"){
#'     outdata <- copy(dfold_flag)
#'     kubename <- .GetKubename(dfold_flag)
#'     datetag <- datetagold
#'     type <- "(old)_FLAGGED.csv"
#'   } else if(filedump == "compareKUBE"){
#'     outdata <- copy(compareKUBE)
#'     kubename <- .GetKubename(dfnew_flag)
#'     kubenameold <- .GetKubename(dfold_flag)
#'     datetag <- data.table::fcase(kubename == kubenameold, paste0(datetagnew, "_vs_", datetagold),
#'                                  kubename != kubenameold, paste0(datetagnew, "_vs_", kubenameold, "_", datetagold))
#'     type <- "COMPARE.csv"
#'   }
#'
#'   # Set filename
#'   if(!is.na(savename)){
#'     filename <- paste0(stringr::str_remove(savename, ".csv"), ".csv")
#'   } else {
#'     filename <- paste0(kubename, "_", datetag, "_", type)
#'   }
#'
#'   file <- paste0(dumppath, filename)
#'
#'   # Write file if it doesn't exist, or if overwrite = TRUE
#'   if(file.exists(file)){
#'     cat(paste0("\nFILEDUMP ", filename, " already exists: "))
#'   }
#'
#'   if(!file.exists(file) || overwrite) {
#'     if(file.exists(file)){
#'       cat("\n---Overwriting existing filedump...---")
#'     }
#'     data.table::fwrite(outdata,
#'                        file = file,
#'                        sep = ";")
#'     cat(paste0("\nFILEDUMP written to folder: ", filename, "\n"))
#'   }
#'
#' }
#'
#' #' .GetKubedatetag
#' #'
#' .GetKubedatetag <- function(data){
#'   stringr::str_extract(attributes(data)$Filename, "\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}")
#' }
#'
#'
#' #' Flag outliers in dfnew_flag
#' #'
#' #' @param data
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' .FlagOutlier <- function(data,
#'                          dims,
#'                          vals){
#'
#'   # Sort file according to dims, final sorting on AAR to create y2y-changes
#'   keyv <- c(stringr::str_subset(dims, "^AAR$", negate = TRUE), "AAR")
#'   data.table::setkeyv(data, c(keyv))
#'
#'   # Check if popinfo exists (weights and geoniv)
#'   if(!exists(".popinfo")){
#'     .popinfo <- .readPopInfo()
#'   }
#'
#'   # Add WEIGHTS and GEOniv columns from .popinfo
#'   data <- collapse::join(data, .popinfo, on = "GEO", how = "left", verbose = 0)
#'
#'   # Identify variable for outlier detection
#'   .val <- data.table::fcase("MEIS" %in% vals, "MEIS",
#'                             "RATE" %in% vals, "RATE",
#'                             "SMR" %in% vals, "SMR",
#'                             "MALTALL" %in% vals, "MALTALL",
#'                             default = NA)
#'
#'   # For rows with data on .val but no weights (can happen if GEO is recoded to e.g. 99),
#'   # weights has to be set to 0 as missing weight is only allowed when value is also missing
#'   data[!is.na(get(.val)) & is.na(WEIGHTS), WEIGHTS := 0]
#'
#'   # If MEIS, RATE or SMR is present, estimate grouped, weighted quantiles and detect outliers
#'   if(!is.na(.val)){
#'
#'     cat(paste0("\n - Outlier detection based on ", .val, "\n"))
#'     data.table::setattr(data, "outliercol", .val)
#'
#'     # Set bycols (geoniv and all dims except GEO/AAR), and create collapse grouping object
#'     bycols <- c("GEOniv", stringr::str_subset(dims, "^(GEO|AAR)$", negate = TRUE))
#'     g <- collapse::GRP(data, bycols)
#'
#'     # Set WEIGHTS to NA if all values are missing in a strata
#'     data[, WEIGHTS := if(all(is.na(get(.val)))){ NA_real_ }, by = bycols]
#'     w <- data$WEIGHTS
#'
#'     # Estimate weighted quantiles, and low and high cutoffs, all values within bycols strata
#'     # Can extract into separate helper functions to create cutoff table, merge cutoffs and define outliers.
#'     cutoffs <- collapse::fmutate(
#'       g[["groups"]],
#'       MIN = collapse::fmin(data[[.val]], g = g),
#'       wq25 = collapse::fnth(data[[.val]], n = 0.25, g = g, w = w, ties = 1),
#'       wq50 = collapse::fnth(data[[.val]], n = 0.50, g = g, w = w, ties = 1),
#'       wq75 = collapse::fnth(data[[.val]], n = 0.75, g = g, w = w, ties = 1),
#'       MAX = collapse::fmax(data[[.val]], g = g),
#'       LOW = wq25 - 1.5*(wq75-wq25),
#'       HIGH = wq75 + 1.5*(wq75-wq25)
#'     )
#'
#'     data <- collapse::join(data, cutoffs, on = bycols, how = "left", overid = 0, verbose = 0)
#'
#'     data[, `:=` (OUTLIER = data.table::fcase(get(.val) < LOW | get(.val) > HIGH, 1,
#'                                              get(.val) >= LOW & get(.val) <= HIGH, 0,
#'                                              default = NA),
#'                  HIGHLOW = data.table::fcase(get(.val) < LOW, "Low",
#'                                              get(.val) > HIGH, "High",
#'                                              default = NA))]
#'
#'     # Create y2y variable (% change), group by GEO instead of GEONIV
#'     .lagval <- paste0("lag", .val)
#'     .changeval <- paste0("change_", .val)
#'     change_bycols <- stringr::str_replace(bycols, "GEOniv", "GEO")
#'
#'     change_g <- collapse::GRP(data, change_bycols)
#'     data[, (.lagval) := collapse::flag(data[, get(.val)], g = change_g)]
#'     data[, (.lagval) := zoo::na.locf(get(.lagval), na.rm = FALSE), by = change_bycols]
#'     data[, (.changeval) := 100*(get(.val)/get(.lagval)-1)]
#'     data[, (.lagval) := NULL]
#'
#'     # Estimate weighted quantiles, and low and high cutoffs, all values within y2ybycols strata
#'
#'     change_cutoffs <- collapse::fmutate(
#'       g[["groups"]],
#'       change_MIN = collapse::fmin(data[[.changeval]], g = g),
#'       change_wq25 = collapse::fnth(data[[.changeval]], n = 0.25, g = g, w = w, ties = 1),
#'       change_wq50 = collapse::fnth(data[[.changeval]], n = 0.50, g = g, w = w, ties = 1),
#'       change_wq75 = collapse::fnth(data[[.changeval]], n = 0.75, g = g, w = w, ties = 1),
#'       change_MAX = collapse::fmax(data[[.changeval]], g = g),
#'       change_LOW = change_wq25 - 1.5*(change_wq75-change_wq25),
#'       change_HIGH = change_wq75 + 1.5*(change_wq75-change_wq25)
#'     )
#'
#'     data <- collapse::join(data, change_cutoffs, on = bycols, overid = 0, verbose = 0)
#'
#'     data[, `:=` (change_OUTLIER = data.table::fcase(get(.changeval) < change_LOW | get(.changeval) > change_HIGH, 1,
#'                                                     get(.changeval) >= change_LOW & get(.changeval) <= change_HIGH, 0,
#'                                                     default = NA),
#'                  change_HIGHLOW = data.table::fcase(get(.changeval) < change_LOW, "Low",
#'                                                     get(.changeval) > change_HIGH, "High",
#'                                                     default = NA))]
#'   } else {
#'     cat("\n- Neither MEIS, RATE, SMR, nor MALTALL available for outlier detection")
#'   }
#' }
#'
#' #' .AddPrevOutlier
#' #'
#' #' Creates a data frame to be used for outlier plotting.
#' #' Adds helper columns outlier_old and newoutlier to filter out new outliers.
#' #'
#' #' @param data1
#' #' @param data2
#' #' @param commondims
#' .AddPrevOutlier <- function(data1,
#'                             data2,
#'                             commondims){
#'
#'   if(attr(data1, "outliercol") != attr(data2, "outliercol")){
#'     cat("Outlier in new file based on ", attr(data1, "outliercol"),
#'         ", and outlier in old file based on ", attr(data2, "outliercol"),
#'         ".\nComparison of outliers not possible...", sep = "")
#'     return(invisible(NULL))
#'   }
#'   d <- copy(data1)
#'   d[data2, `:=` (PREV_OUTLIER = i.OUTLIER,
#'                  change_PREV_OUTLIER = i.change_OUTLIER),
#'     on = commondims]
#'
#'   collapse::settransform(d,
#'                          NEW_OUTLIER = 0,
#'                          change_NEW_OUTLIER = 0)
#'
#'   d[OUTLIER == 1 & (is.na(PREV_OUTLIER) | PREV_OUTLIER == 0), NEW_OUTLIER := 1]
#'   d[change_OUTLIER == 1 & (is.na(change_PREV_OUTLIER) | change_PREV_OUTLIER == 0), change_NEW_OUTLIER := 1]
#'
#'   # Move all change columns to the end
#'   allnames <- names(d)
#'   data.table::setcolorder(d, c(stringr::str_subset(allnames, "change_", negate = TRUE),
#'                                stringr::str_subset(allnames, "change_", negate = FALSE)))
#'
#'   d[]
#'
#' }
#'
#'
#'
#'
#'
#'
#' .find_compare <- function(data,
#'                           type){
#'   if(type == "TELLER"){
#'     val <- data.table::fcase("sumTELLER_uprikk" %in% names(data), "sumTELLER_uprikk",
#'                              "sumTELLER" %in% names(data), "sumTELLER",
#'                              "TELLER_uprikk" %in% names(data), "TELLER_uprikk",
#'                              "TELLER" %in% names(data), "TELLER",
#'                              default = NA_character_)
#'   }
#'
#'   if(type == "NEVNER"){
#'     val <- data.table::fcase("sumNEVNER_uprikk" %in% names(data), "sumNEVNER_uprikk",
#'                              "sumNEVNER" %in% names(data), "sumNEVNER",
#'                              "NEVNER_uprikk" %in% names(data), "NEVNER_uprikk",
#'                              "NEVNER" %in% names(data), "NEVNER",
#'                              default = NA_character_)
#'   }
#'
#'   val
#' }
#'
#' .IdentifyColumns <- function(data1 = NULL,
#'                              data2 = NULL){
#'
#'   if(is.null(data1)){
#'     stop("data1 not provided in .IdentifyColumns()")
#'   }
#'
#'   if(!exists(".ALL_DIMENSIONS")) {
#'     source("https://raw.githubusercontent.com/helseprofil/misc/main/alldimensions.R")
#'     .ALL_DIMENSIONS <- ALL_DIMENSIONS
#'   }
#'
#'   .dims1 <<- names(data1)[names(data1) %in% .ALL_DIMENSIONS]
#'   .vals1 <<- stringr::str_subset(names(data1), stringr::str_c("\\b",.dims1, "\\b", collapse = "|"), negate = T)
#'
#'   # Create objects relevant for data2
#'   .dims2 <<- NULL
#'   .vals2 <<- NULL
#'   .commondims <<- NULL
#'   .newdims <<- NULL
#'   .expdims <<- NULL
#'   .commonvals <<- NULL
#'   .newvals <<- NULL
#'   .expvals <<- NULL
#'   .commoncols <<- NULL
#'
#'   # If second data is provided, replace objects above
#'   if(!is.null(data2)){
#'     .dims2 <<- names(data2)[names(data2) %in% .ALL_DIMENSIONS]
#'     .vals2 <<- stringr::str_subset(names(data2), stringr::str_c(.dims2, collapse = "|"), negate = T)
#'     .commondims <<- .dims1[.dims1 %in% .dims2]
#'     .newdims <<- stringr::str_subset(.dims1, stringr::str_c("\\b", .dims2, "\\b", collapse = "|"), negate = T)
#'     .expdims <<- stringr::str_subset(.dims2, stringr::str_c("\\b", .dims1, "\\b", collapse = "|"), negate = T)
#'     .commonvals <<- .vals1[.vals1 %in% .vals2]
#'     .newvals <<- stringr::str_subset(.vals1, stringr::str_c("\\b", .vals2, "\\b", collapse = "|"), negate = T)
#'     .expvals <<- stringr::str_subset(.vals2, stringr::str_c("\\b", .vals1, "\\b", collapse = "|"), negate = T)
#'     .commoncols <<- c(.commondims, .commonvals)
#'   }
#' }
