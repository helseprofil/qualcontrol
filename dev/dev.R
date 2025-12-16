# DEV

# Read test files

readfiles(cube.new = "UFORE_1_UTDANN_2025-12-11-11-25",
          cube.old = "UFORE_1_2025-02-24-13-32",
          recode.old = T,
          comparecube = T,
          outliers = T,
          dumps = NULL)

# dt <- data.table::copy(comparecube)


# cube = data.table::copy(newcube)
# cube.new = data.table::copy(newcube)
# cube.old = data.table::copy(oldcube)
#
# colinfo <- identify_coltypes(cube.new, cube.old)


