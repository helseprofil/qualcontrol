library(microbenchmark)

d <- data.table::copy(newcube)


microbenchmark(ref = {
  d <- data.table::copy(newcube)
  get_complete_strata(d, c("GEO", "ALDER", "BODD"))
},
  copy = {
    d <- data.table::copy(newcube)
    get_complete_strata2(d, c("GEO", "ALDER", "BODD"))
    }, times = 1000)


