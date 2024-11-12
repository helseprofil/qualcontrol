test_that("barometer check works", {
  invisible(capture.output(expect_no_error(check_barometer("FHP", 2024, "fylke"))))
  invisible(capture.output(expect_no_error(check_barometer("FHP", 2024, "kommune"))))
  invisible(capture.output(expect_no_error(check_barometer("FHP", 2024, "bydel"))))
  invisible(capture.output(expect_no_error(check_barometer("OVP", 2024, "kommune"))))
  invisible(capture.output(expect_no_error(check_barometer("OVP", 2024, "bydel"))))

  invisible(capture.output(expect_error(check_barometer("OVP", 2024, "fylke"))))
  invisible(capture.output(expect_error(check_barometer("FHP", year = NULL))))
  invisible(capture.output(expect_error(check_barometer(year = 2024, geo = "land"))))

})
