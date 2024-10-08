# DEV

# Read test files

# TRANGBODD
readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
          modus.new = "KH",
          cube.old = "TRANGBODDHET_2023-01-03-14-38",
          modus.old = "KH",
          recode.old = T)

make_comparecube(newcube, oldcube, dumps = NULL)

# HKR
readfiles(cube.new = "HKR_2024-02-01-12-42",
          modus.new = "KH",
          cube.old = "HKR_2023-09-20-14-52",
          modus.old = "KH",
          recode.old = T)

# dt <- data.table::copy(comparecube)


# cube = data.table::copy(newcube)
# cube.new = data.table::copy(newcube)
# cube.old = data.table::copy(oldcube)
#
# colinfo <- identify_coltypes(cube.new, cube.old)


