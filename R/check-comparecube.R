#' @title comparecube_summary
#' @description
#' Generate a table estimating, for each strata by GEOniv/value column,
#' the number of identical/different rows, new/expired censored observations,
#' and mean/min/max diff / reldiff.
#'
#' Uses [qualcontrol::qc_round()] on the data to eliminate very marginal differences.
#' @param dt comparecube
#'
#' @return a DT output table
#' @export
comparecube_summary <- function(dt = comparecube){

  if(is.null(dt)) return(invisible(NULL))

  dt <- data.table::copy(dt[newrow == 0]) |> qc_round() |> translate_geoniv()
  diffvals <- gsub("_diff", "", grep("_diff$", names(dt), value = T))
  tabvals <- c("Identical", "Different", "New_prikk", "Expired_prikk",
               "Mean_diff", "Min_diff", "Max_diff",
               "Mean_reldiff", "Min_reldiff", "Max_reldiff")
  data.table::setkeyv(dt, c("GEOniv", data.table::key(dt)))
  geolevels <- c("TOTAL", levels(dt$GEOniv))

  out <- data.table::CJ(GEOniv = forcats::fct_inorder(geolevels),
                        Value = forcats::fct_inorder(diffvals))
  out[, (tabvals) := NA_real_]

  for(geolevel in geolevels){
    subset <- dt
    if(geolevel != "TOTAL"){ subset <- subset[GEOniv == geolevel] }
    summarise_diffvals(out, subset, diffvals, geolevel)
  }

  nosearch <- grep("^GEOniv$|^Value$", names(out), invert = T, value = T)

  return(tab_output(out,
                    nosearchcolumns = nosearch))
}

#' @title summarise_diffvals
#' @keywords internal
#' @noRd
#' @description
#' Helper function for [qualcontrol::compare_diffrows()].
#' For each strata by GEOniv/value column, calculates number of identical/different rows,
#' new/expired censored observations, and mean/min/max diff / reldiff
#' Calculates diffs by reference, no need to reassign.
summarise_diffvals <- function(out,
                               subset,
                               diffvals,
                               geolevel){

  for(value in diffvals){
    new <- paste0(value, "_new")
    old <- paste0(value, "_old")
    diff <- paste0(value, "_diff")
    reldiff <- paste0(value, "_reldiff")
    calculate_reldiff <- reldiff %in% names(subset)

    identical <- subset[get(diff) == 0, .N]
    different <- subset[get(diff) != 0, .N]
    newprikk <- subset[is.na(get(new)) & !is.na(get(old)), .N]
    expprikk <- subset[!is.na(get(new)) & is.na(get(old)), .N]

    out[GEOniv == geolevel & Value == value, let(Identical = identical,
                                              Different = different,
                                              New_prikk = newprikk,
                                              Expired_prikk = expprikk)]

    if(different > 0){
      diffdata <- subset[get(diff) != 0 & !is.na(get(new)) & !is.na(get(old))]
      out[GEOniv == geolevel & Value == value, let(Mean_diff = round(mean(diffdata[[diff]], na.rm = T), 3),
                                                   Min_diff = round(min(diffdata[[diff]], na.rm = T), 3),
                                                   Max_diff = round(max(diffdata[[diff]], na.rm = T), 3))]
    }

    if(different > 0 & calculate_reldiff){
      out[GEOniv == geolevel & Value == value, let(Mean_reldiff = round(mean(diffdata[[reldiff]], na.rm = T), 3),
                                                   Min_reldiff = round(min(diffdata[[reldiff]], na.rm = T), 3),
                                                   Max_reldiff = round(max(diffdata[[reldiff]], na.rm = T), 3))]
    }
  }
}
