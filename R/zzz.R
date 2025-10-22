## Global options
opt.qualcontrol <- orgdata:::is_globs("qualcontrol")

.onLoad <- function(libname, pkgname) {
  optqualcontrol <- orgdata:::is_globs("qualcontrol")
  orgDT <- !(names(optqualcontrol) %in% names(options()))
  if (any(orgDT)) options(optqualcontrol[orgDT])

  corrglobs <- orgdata:::is_correct_globs(optqualcontrol)
  if(!isTRUE(corrglobs)){
    x <- utils::menu(title = "Options are not the same as in the config file, update options now?",
                     choices = c("Yes", "No"))
    if(x == 1){
      orgdata:::update_globs("qualcontrol")
    }
  }

  alldims <- update_dimlist()
  currentdimlist <- getOption("qualcontrol.alldimensions")
  missingdims <- alldims[!alldims %in% currentdimlist]
  if(length(missingdims) > 0){
    warning(paste0("\nDimensions missing from dimlist in options: ", paste(missingdims, collapse = ", "),
                   "\nUpdating options for now, but the list should be updated in `backend::config-qualcontrol.yml`\n"))
    options(qualcontrol.alldimensions = c(currentdimlist, missingdims))
  }

  invisible()
}

.onAttach <- function(libname, pkgname){
  newver <- suppressMessages(orgdata:::is_latest_version("qualcontrol"))
  if (newver){
    x <- utils::askYesNo("Update qualcontrol now?")
    if (isTRUE(x)){
      orgdata::update_khpackage("qualcontrol")
    }
  }
  packageStartupMessage("qualcontrol version: ",
                        utils::packageDescription("qualcontrol")[["Version"]],
                        "\n- Population file used for weighting and geo-level: ", attributes(.popinfo)$popfile,
                        "\n- Geo recode year: ", attributes(.georecode)$year)
}
