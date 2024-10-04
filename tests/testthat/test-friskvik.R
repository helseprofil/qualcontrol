test_that("check_friskvik works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping friskvik-test")
  invisible(capture.output(expect_no_error(check_friskvik("FHP","F", "2024", test = T))))
})
