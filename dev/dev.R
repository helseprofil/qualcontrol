# DEV

# Read test files
system.time({
readfiles(cube.new = "BEFOLK_GK_2026-06-16-13-23",
          cube.old = "BEFOLK_GK_2025-10-21-10-34",
          recode.old = T,
          recode.new = F,
          comparecube = F,
          outliers = T,
          dumps = NULL,
          useduck = T)
})

# dt <- data.table::copy(comparecube)


# cube = data.table::copy(newcube)
# cube.new = data.table::copy(newcube)
# cube.old = data.table::copy(oldcube)
#
# colinfo <- identify_coltypes(cube.new, cube.old)


