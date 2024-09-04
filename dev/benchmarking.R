library(microbenchmark)




microbenchmark(
  preallocate = {
    comparecube <- combine_cubes(newcube_flag, oldcube_flag, colinfo)
    add_diffcolumns(comparecube, commonvals)
    },

  expand = {
    comparecube <- combine_cubes(newcube_flag, oldcube_flag, colinfo)
    add_diffcolumns2(comparecube, commonvals)
    },
  times = 50)


