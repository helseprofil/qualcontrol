test_that("internal objects exists", {
  expect_true(exists(".validgeo"))
  expect_type(.validgeo, "double")
  expect_true(exists(".georecode"))
  expect_type(.georecode, "list")
  expect_true(exists(".popinfo"))
  expect_type(.popinfo, "list")
})

test_that("lookup tabs exist", {
  expect_true(file.exists(system.file("data", "popinfo.rds", package = "qualcontrol")))
  expect_true(file.exists(system.file("data", "georecode.rds", package = "qualcontrol")))
})


test_that("update_georecode works", {
  invisible(capture.output(expect_no_error(update_georecode(2024, overwrite = F))))
})

test_that("update_popinfo works", {
  invisible(capture.output(expect_no_error(update_popinfo("O:/Prosjekt/FHP/PRODUKSJON/PRODUKTER/KUBER/KOMMUNEHELSA/DATERT/csv/BEFOLK_GK_2025-03-03-15-59.csv",
                                                          overwrite = F))))
})

test_that("update_internal_data works", {
  expect_no_error(update_internal_data(2024, overwrite = FALSE))
})
