# DEV

# Read test files

# TRANGBODD
readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
          modus.new = "KH",
          cube.old = "TRANGBODDHET_2023-01-03-14-38",
          modus.old = "KH",
          recode.old = T)

# HKR
readfiles(cube.new = "HKR_2024-02-01-12-42",
          modus.new = "KH",
          cube.old = "HKR_2022-12-14-12-18",
          modus.old = "KH",
          recode.old = T)


cube = data.table::copy(newcube)
cube.new = data.table::copy(newcube)
cube.old = data.table::copy(oldcube)

# TO-DO
# - Make function to save report
