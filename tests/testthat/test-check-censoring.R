invisible(capture.output(cube1 <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
invisible(capture.output(cube2 <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))

test_that("check_censoring works", {
  expect_error(withr::with_locale(c("LC_CTYPE" = "nb-NO.UTF-8"),
                                  check_censoring(cube1)),
               regexp = ".*not found in ACCESS::KUBER")
})

test_that("compare_censoring works", {
  expect_error(compare_censoring(NULL, NULL), regexp = "cube.new must be provided")
  # expect_type(compare_censoring(cube1, NULL), "list")
  # expect_type(compare_censoring(cube1, cube2), "list")
  # tests fails due to writing of csv-files
})

test_that("compare_censoring_timeseries works", {
  expect_error(compare_censoring_timeseries(NULL, NULL), regexp = "cube.new must be provided")
  # expect_no_error(compare_censoring_timeseries(cube1, NULL))
  # expect_no_error(compare_censoring_timeseries(cube1, cube2))
  # tests fails due to writing of csv-files
})
