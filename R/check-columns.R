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


