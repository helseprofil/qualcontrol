# Sets newcube-attribute Filename to "TESTMAPPE", to print plots to TEST-folder.

invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = "TRANGBODDHET_2023-01-03-14-38",
                                   modus.old = "KH",
                                   recode.old = T)))
data.table::setattr(newcube, "Filename", "TESTMAPPE")
invisible(capture.output(make_comparecube(newcube, oldcube, dumps = NULL)))

test_that("plot_timeseries_country", {
  generate_qcfolders("TESTMAPPE")
  expect_no_error(plot_timeseries_country(newcube))
})

test_that("plot_boxplot works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping plot_boxplot-test")
  expect_no_error(plot_boxplot(onlynew = TRUE, change = FALSE, save = FALSE))
  expect_no_error(plot_boxplot(onlynew = TRUE, change = TRUE, save = FALSE))
  expect_no_error(plot_boxplot(onlynew = FALSE, change = FALSE, save = FALSE))
  expect_no_error(plot_boxplot(onlynew = FALSE, change = TRUE, save = FALSE))
})

test_that("plot_timeseries works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping plot_timeseries-test")
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = TRUE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = TRUE, change = TRUE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = FALSE, change = FALSE, save = FALSE))))
  invisible(capture.output(expect_no_error(plot_timeseries(onlynew = FALSE, change = TRUE, save = FALSE))))
})
