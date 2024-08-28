library(microbenchmark)




microbenchmark(
  old = {
    .FlagNew(cube.new, cube.old, colinfo$commondims, colinfo$newdims,colinfo$dims.new, colinfo$vals.new, F)
    .FlagOld(cube.new, cube.old, colinfo$commondims, colinfo$expdims,colinfo$dims.new, colinfo$vals.new, F)
    },

  new = {
    make_comparecube(cube.new, cube.old)
    },
  times = 100)


