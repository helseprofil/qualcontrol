library(microbenchmark)



microbenchmark(
  old = {
    compare_dimensions(cube.new, cube.old)
    },
  new = {
    compare_dimensions2(cube.new, cube.old)
    },
  times = 5000)


