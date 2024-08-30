# Sets attribute Filename to "TESTMAPPE", to print plots to TEST-folder.

invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = NULL)))

test_that("plot_timeseries_country", {
  data.table::setattr(newcube, "Filename", "TESTMAPPE")
  generate_qcfolders("TESTMAPPE")
  expect_no_error(plot_timeseries_country(newcube))
})
