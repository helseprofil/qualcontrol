invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = NULL)))

test_that("compare_geolevels", {
  expect_equal(compare_geolevels(newcube, "FL"), compare_geolevels(newcube, "LF"))
  expect_equal(compare_geolevels(newcube, "KF"), compare_geolevels(newcube, "FK"))
  expect_equal(compare_geolevels(newcube, "BK"), compare_geolevels(newcube, "KB"))
})

test_that("unknown_bydel works", {
  invisible(capture.output(expect_no_error(unknown_bydel(newcube))))
  invisible(capture.output(expect_no_error(unknown_bydel(newcube, maxrows = F))))
  invisible(capture.output(expect_equal(unknown_bydel(newcube), unknown_bydel(newcube, maxrows = F))))
  invisible(capture.output(expect_null(unknown_bydel(newcube[GEO < 9999]))))
  invisible(capture.output(expect_null(unknown_bydel(newcube[, .(GEO, AAR, ALDER, RATE, SMR, SPVFLAGG)]))))
})
