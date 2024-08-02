test_that("internal objects exists", {
  expect_true(exists(".validdims"))
  expect_type(.validdims, "character")
})

test_that("lookup tabs exist", {
  expect_true(file.exists(system.file("data", "popinfo.rds", package = "qualcontrol")))
  expect_true(file.exists(system.file("data", "georecode.rds", package = "qualcontrol")))
})
