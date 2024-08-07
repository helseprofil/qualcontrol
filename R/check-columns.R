#' @title CompareCols
#' @description
#' Compare the columns across two cubes files
#' Report whether any new (not in old), or expired (not in new) columns are present.
#'
#' @param data1 New cube
#' @param data2 Old cube or NULL
#' @export
compare_colnames <- function(cube.new = NULL,
                             cube.old = NULL){

  if(is.null(cube.new)) stop("cube.new must be provided")

  if(is.null(cube.old)){
    cat(paste0("Columns in new cube: ", paste(names(cube.new), collapse = ", ")))
    return(invisible(NULL))
  }

  newcols <- names(cube.new)[names(cube.new) %!in% c(names(cube.old), "origgeo") & !grepl("_uprikk", names(cube.new))]
  expcols <- names(cube.old)[names(cube.old) %!in% c(names(cube.new), "origgeo") & !grepl("_uprikk", names(cube.old))]
  uprikkcols <- grep("_uprikk$", names(cube.new), value = T)

  msgnew <- data.table::fcase(length(newcols) == 0, "-No new columns.",
                              default = paste0("-New columns found: ", paste(newcols, collapse = ", ")))
  msgexp <- data.table::fcase(length(expcols) == 0, "\n-No expired columns.",
                              default = paste0("\n-Expired columns found: ", paste(expcols, collapse = ", ")))
  msguprikk <- data.table::fcase(length(uprikkcols) == 0, "\n  -No '_uprikk'-columns in new file",
                                 default = paste0("\n-New file _uprikk columns: ", paste(uprikkcols, collapse = ", ")))

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
compare_dimensions <- function(cube.new = NULL,
                               cube.old = NULL){

  if(is.null(cube.new)) stop("cube.new must be provided")
  colinfo <- identify_coltypes(cube.new, cube.old)

  out <- list(Dimension = colinfo$dims.new)

  n_lev <-  integer()
  for(i in out$Dimension) {
    n_lev <- c(n_lev, length(unique(cube.new[[i]])))
  }
  out[["N levels"]] <- n_lev

  if(!is.null(cube.old)){
    new_lev <- character()
    exp_lev <- character()

    for(i in out$Dimension){
      new <- unique(cube.new[[i]][cube.new[[i]] %notin% cube.old[[i]]])
      exp <- unique(cube.old[[i]][cube.old[[i]] %notin% cube.new[[i]]])
      new_lev <- c(new_lev, ifelse(length(new) == 0, "", paste0(new, collapse = ", ")))
      exp_lev <- c(exp_lev, ifelse(length(exp) == 0, "", paste0(exp, collapse = ", ")))
    }

    out[["New levels"]] <- new_lev
    out[["Expired levels"]] <- exp_lev
  }
  return(data.table::as.data.table(out))
}
