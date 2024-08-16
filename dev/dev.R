# DEV

# Read test files

# TRANGBODD
readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
          modus.new = "KH",
          cube.old = "TRANGBODDHET_2023-01-03-14-38",
          modus.old = "KH",
          recode.old = T)


cube = data.table::copy(newcube)
cube.new = data.table::copy(newcube)
cube.old = data.table::copy(oldcube)



# TO-DO
# - Consider saving plots directly in the plots folder, instead of in reports.
#   - Will make the reports smaller.
#   - Will make sizing of the plotting area easier
# - Make function to save report
