# DEV

# Read test files

# TRANGBODD
readfiles(cube.new = "FLx_UTDN_DIFF_15_2025-01-13-09-56",
          modus.new = "KH",
          cube.old = "FLx_UTDN_DIFF_15_2025-01-09-16-17",
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


