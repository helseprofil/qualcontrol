test_that("compare_colnames", {

  invisible(capture.output(new <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
  invisible(capture.output(old <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))

  invisible(capture.output(expect_no_error(compare_colnames(new, old))))
})

