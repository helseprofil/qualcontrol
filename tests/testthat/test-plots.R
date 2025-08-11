# Sets newcube-attribute Filename to "TESTMAPPE", to print plots to TEST-folder.

suppressWarnings(invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2025-02-25-08-47",
                                                    cube.old = "TRANGBODDHET_2023-11-20-11-05",
                                                    recode.old = T))))
data.table::setattr(newcube, "Filename", "TESTMAPPE")
invisible(capture.output(make_comparecube(newcube, oldcube, dumps = NULL)))

test_that("plot_timeseries_country works", {
  expect_no_error(plot_timeseries_country(newcube, save = FALSE))
})

test_that("plot_boxplot works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping plot_boxplot-test")
  invisible(capture.output(expect_no_error(plot_boxplot(onlynew = TRUE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_boxplot(onlynew = TRUE, change = TRUE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_boxplot(onlynew = FALSE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_boxplot(onlynew = FALSE, change = TRUE, save = FALSE))))
})

test_that("plot_timeseries works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping plot_timeseries-test")
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = TRUE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = TRUE, change = TRUE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = FALSE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = FALSE, change = TRUE, save = FALSE))))
})

test_that("plot_timeseries_bydel works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping plot_timeseries-test")
  invisible(capture.output(expect_no_error(plot_timeseries_bydel(save = FALSE))))
})
