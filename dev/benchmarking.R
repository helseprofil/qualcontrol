library(microbenchmark)

microbenchmark(old = ComparePrikkTS(newcube, oldcube),
               new = compare_censoring_timeseries(newcube, oldcube),
               times = 10)


