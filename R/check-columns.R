#' @title CompareCols
#' @description
#' Compare the columns across two cubes files
#' Report whether any new (not in old), or expired (not in new) columns are present.
#'
#' @param data1 New cube
#' @param data2 Old cube or NULL
#' @export
compare_colnames <- function(cube.new = newcube,
                             cube.old = oldcube){

  if(is.null(cube.new)) stop("cube.new must be provided")

  if(is.null(cube.old)){
    cat(paste0("Columns in new cube: ", paste(names(cube.new), collapse = ", ")))
    return(invisible(NULL))
  }

  misccols <- c("GEOniv", "WEIGHTS", "origgeo")

  cube.new.cols <- names(cube.new)[names(cube.new) %notin% misccols & !grepl("_uprikk", names(cube.new))]
  newcols <- cube.new.cols[cube.new.cols %notin% c(names(cube.old))]
  expcols <- names(cube.old)[names(cube.old) %notin% c(names(cube.new), misccols) & !grepl("_uprikk", names(cube.old))]
  uprikkcols <- grep("_uprikk$", names(cube.new), value = T)

  msgnew <- data.table::fcase(length(newcols) == 0, "\n- No new columns.",
                              default = paste0("\n- New columns found: ", paste(newcols, collapse = ", ")))
  msgexp <- data.table::fcase(length(expcols) == 0, "\n- No expired columns.",
                              default = paste0("\n- Expired columns found: ", paste(expcols, collapse = ", ")))
  msguprikk <- data.table::fcase(length(uprikkcols) == 0, "\n-No '_uprikk'-columns in new file",
                                 default = paste0("\n- New file _uprikk columns: ", paste(uprikkcols, collapse = ", ")))

  cat("- Columns in new file: ", paste(cube.new.cols, collapse = ", "))
  cat(msgnew)
  cat(msgexp)
  cat(msguprikk)
}

#' @title compare_dimensions
#' @description
#' Compares the unique levels of each dimension in the new file.
#' Checks if any levels are new (not in the old file) or expired (not in the new file).
#'
#' @param cube.new New cube
#' @param cube.old Old cube, or NULL
#'
#' @export
compare_dimensions <- function(cube.new = newcube,
                               cube.old = oldcube){

  if(is.null(cube.new)) stop("cube.new must be provided")
  colinfo <- identify_coltypes(cube.new, cube.old)

  out <- list(Dimension = colinfo$dims.new)
  dimlength <- length(out$Dimension)

  n_lev <- integer(dimlength)
  for(i in 1:dimlength) {
    dim <- out$Dimension[i]
    n_lev[i] <- length(unique(cube.new[[dim]]))
  }
  out[["N levels"]] <- n_lev

  if(!is.null(cube.old)){
    new_lev <- character(dimlength)
    exp_lev <- character(dimlength)

    for(i in 1:dimlength){
      dim <- out$Dimension[i]
      new <- unique(cube.new[[dim]][cube.new[[dim]] %notin% cube.old[[dim]]])
      exp <- unique(cube.old[[dim]][cube.old[[dim]] %notin% cube.new[[dim]]])
      new_lev[i] <- ifelse(length(new) == 0, "", paste0(new, collapse = ", "))
      exp_lev[i] <- ifelse(length(exp) == 0, "", paste0(exp, collapse = ", "))
    }

    out[["New levels"]] <- new_lev
    out[["Expired levels"]] <- exp_lev
  }
  return(data.table::as.data.table(out))
}
