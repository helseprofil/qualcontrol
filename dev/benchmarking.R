library(microbenchmark)

GROUPdims = c("AAR", "ALDER", "BODD")
CompareGEO = TRUE

microbenchmark(oldt = select_teller_pri(names(newcube)),
               oldn = select_nevner_pri(names(newcube)),
               times = 1000
               )


