test_that("ConnectKHelsa works", {
  expect_no_error(con <- ConnectKHelsa())
  expect_no_error("KUBER" %in% RODBC::sqlTables(con)$TABLE_NAME)
  RODBC::odbcCloseAll()
})

test_that("ConnectGeoKoder works", {
  expect_no_error(con <- ConnectGeokoder())
  expect_no_error("tblGeo" %in% RODBC::sqlTables(con)$TABLE_NAME)
  RODBC::odbcCloseAll()
})

test_that("SQLstring and SQLdate works", {
  expect_equal(SQLstring("TEST"), "'TEST'")
  expect_equal(SQLdate(as.Date("28082024", "%d%m%Y")), "#2024-08-28#")
})
