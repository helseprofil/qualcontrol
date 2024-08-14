test_that("internal objects exists", {
  expect_true(exists(".validdims"))
  expect_type(.validdims, "character")
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
