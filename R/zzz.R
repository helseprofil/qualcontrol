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
