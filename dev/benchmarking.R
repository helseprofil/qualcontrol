library(microbenchmark)

d <- data.table::copy(newcube)


microbenchmark(ref = {
  d <- data.table::copy(newcube)
  d <- d[, c(..colinfo[["dims.new"]], ..plotvals)]
},
  new = {
    d <- data.table::copy(newcube)
    d <- d[, mget(c(colinfo$dims.new, plotvals))]
    },
  new2 = {
    d <- data.table::copy(newcube)
    d <- d[, mget(c(colinfo[["dims.new"]], plotvals))]
    },
new3 = {
  d <- data.table::copy(newcube)
  alldims <- colinfo$dims.new
  d <- d[, c(..alldims, ..plotvals)]
}, times = 500)


