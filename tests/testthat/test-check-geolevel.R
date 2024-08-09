invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = "TRANGBODDHET_2023-01-03-14-38",
                                   modus.old = "KH",
                                   recode.old = T)))

test_that("compare_geolevels", {

  expect_equal(compare_geolevels(newcube, "FL"), compare_geolevels(newcube, "LF"))
  expect_equal(compare_geolevels(newcube, "KF"), compare_geolevels(newcube, "FK"))
  expect_equal(compare_geolevels(newcube, "BK"), compare_geolevels(newcube, "KB"))


})
