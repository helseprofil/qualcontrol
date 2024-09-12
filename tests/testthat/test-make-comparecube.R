invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = "TRANGBODDHET_2023-01-03-14-38",
                                   modus.old = "KH",
                                   recode.old = T)))

test_that("make_comparecube works", {

  orgyear <- getOption("qualcontrol.year")
  options(qualcontrol.year = "TEST")
  expect_no_error(make_comparecube(newcube, oldcube, outliers = F, dumps = NULL))
  expect_no_error(make_comparecube(newcube, oldcube, outliers = T, dumps = NULL))

  single_aar = "2021_2021"
  invisible(capture.output(expect_no_error(make_comparecube(newcube[AAR == single_aar], oldcube[AAR == single_aar], outliers = F, overwrite = T))))
  invisible(capture.output(expect_no_error(make_comparecube(newcube[AAR == single_aar], oldcube[AAR == single_aar], outliers = T, overwrite = T))))
  options(qualcontrol.year = orgyear)
})

test_that("select_outlier_pri works", {
  d1 <- create_empty_standard_dt()
  d2 <- data.table::copy(d1)[, let(MEIS = NULL)]
  d3 <- data.table::copy(d2)[, let(RATE = NULL)]
  d4 <- data.table::copy(d3)[, let(SMR = NULL)]

  colinfo1 <- identify_coltypes(d1)
  colinfo2 <- identify_coltypes(d1, d2)
  colinfo3 <- identify_coltypes(d1, d3)
  colinfo4 <- identify_coltypes(d1, d4)

  expect_no_error(outlier1 <- select_outlier_pri(d1, NULL, colinfo1))
  expect_equal(outlier1, "MEIS")
  expect_no_error(outlier2 <- select_outlier_pri(d1, d2, colinfo2))
  expect_equal(outlier2, "RATE")
  expect_no_error(outlier3 <- select_outlier_pri(d1, d3, colinfo3))
  expect_equal(outlier3, "SMR")
  expect_no_error(outlier4 <- select_outlier_pri(d1, d4, colinfo4))
  expect_true(is.na(outlier4))
  expect_error(select_outlier_pri(NULL), regexp = "'cube.new' must be provided")
})

test_that("flag_rows works", {
  colinfo <- identify_coltypes(newcube, oldcube)
  expect_no_error(flag_rows(newcube, oldcube, colinfo, "newrow"))
  expect_no_error(flag_rows(newcube, oldcube, colinfo, "exprow"))

  onlynew <- flag_rows(newcube, NULL, colinfo, "newrow")
  expect_equal(unique(onlynew$newrow), 1)

  expect_error(flag_rows(NULL, oldcube, colinfo = colinfo, "newrow"), regexp = "cube.new cannot be NULL when flag = 'newrow'")
  expect_error(flag_rows(NULL, oldcube, colinfo = colinfo, "exprow"), regexp = "cube.new and cube.old cannot be NULL when flag = 'exprow'")
  expect_error(flag_rows(newcube, NULL, colinfo = colinfo, "exprow"), regexp = "cube.new and cube.old cannot be NULL when flag = 'exprow'")
})
