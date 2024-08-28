invisible(capture.output(new <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
invisible(capture.output(old <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))

test_that("compare_colnames", {

  invisible(capture.output(expect_no_error(compare_colnames(new, old))))
  invisible(capture.output(expect_null(compare_colnames(new, NULL))))
  invisible(capture.output(expect_error(compare_colnames(NULL, NULL))))

})

test_that("compare_dimensions works", {
  expect_error(compare_dimensions(NULL, NULL), regexp = "cube.new must be provided")
  expect_no_error(compare_dimensions(new, old))
})

