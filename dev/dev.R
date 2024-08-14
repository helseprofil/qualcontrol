# DEV

# Read test files

readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
          modus.new = "KH",
          cube.old = "TRANGBODDHET_2023-01-03-14-38",
          modus.old = "KH",
          recode.old = T)


cube = data.table::copy(newcube)
cube.new = data.table::copy(newcube)
cube.old = data.table::copy(oldcube)



