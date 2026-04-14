# library(microbenchmark)
#
microbenchmark(
  Old = {
    fun2(d, bycols, quantiles, limits)
    },

  New = {
    fun1(d, bycols, quantiles, limits)
    },
  times = 25)
#
#
