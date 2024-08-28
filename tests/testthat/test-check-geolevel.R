invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = NULL)))

test_that("compare_geolevels works", {
  expect_equal(compare_geolevels(newcube, "FL"), compare_geolevels(newcube, "LF"))
  expect_equal(compare_geolevels(newcube, "KF"), compare_geolevels(newcube, "FK"))
  expect_equal(compare_geolevels(newcube, "BK"), compare_geolevels(newcube, "KB"))
  expect_no_error(compare_geolevels(newcube, "OO"))

  test <- data.table::copy(newcube)[, (grep("TELLER", names(newcube), value = T)) := NULL]
  invisible(capture.output(expect_null(compare_geolevels(test, "FL"))))

  test2 <- data.table::copy(newcube)[GEOniv != "L"]
  expect_error(compare_geolevels(test2, "FL"), regexp = "Only 1 of the required geolevels present")
})

test_that("unknown_bydel works", {
  invisible(capture.output(expect_no_error(unknown_bydel(newcube))))
  invisible(capture.output(expect_no_error(unknown_bydel(newcube, crop = F))))
  invisible(capture.output(expect_no_error(unknown_bydel(newcube, crop = T, maxrows = 100))))
  invisible(capture.output(expect_equal(unknown_bydel(newcube), unknown_bydel(newcube, crop = F))))
  invisible(capture.output(expect_null(unknown_bydel(newcube[GEO < 9999]))))
  invisible(capture.output(expect_error(unknown_bydel(newcube[, .(GEO, AAR, ALDER, RATE, SMR, SPVFLAGG)]))))

  test3 <- data.table::copy(newcube)[, (grep("TELLER|NEVNER", names(newcube), value = T)) := NULL]
  invisible(capture.output(expect_null(unknown_bydel(test3))))

  test4 <- data.table::copy(newcube)[, (grep("TELLER|NEVNER", names(newcube), value = T)) := NA]
  invisible(capture.output(expect_null(unknown_bydel(test4))))
})
