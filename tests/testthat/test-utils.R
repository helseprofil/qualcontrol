invisible(capture.output(cube1 <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
invisible(capture.output(cube2 <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))

test_that("identify_coltypes works", {

  expect_error(identify_coltypes(cube.new = NULL, cube.old = NULL),
               regexp = "cube.new must be provided")

  expect_type(identify_coltypes(cube1, cube2), "list")
  expect_length(identify_coltypes(cube1, cube2), 13)
  expect_length(identify_coltypes(cube1, NULL), 3)
})

test_that("get_cubename works", {
  expect_equal(get_cubename(cube1), "testcube1")
})

test_that("generate_qcfolders",code =  {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping generate_qcfolders-test")

  generate_qcfolders("TESTFOLDERS", "TESTKUBENAVN")
  testroot <- file.path(getOption("qualcontrol.root"),
                        getOption("qualcontrol.output"),
                        "TESTFOLDERS")

  basepath <- file.path(testroot, "TESTKUBENAVN")
  expect_true(
    all(dir.exists(basepath),
        dir.exists(file.path(basepath, "arkiv")),
        dir.exists(file.path(basepath, "FILDUMPER")),
        dir.exists(file.path(basepath, "FILDUMPER", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT")),
        dir.exists(file.path(basepath, "PLOTT", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT", "BP")),
        dir.exists(file.path(basepath, "PLOTT", "BP", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT", "BPc")),
        dir.exists(file.path(basepath, "PLOTT", "BPc", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT", "TS")),
        dir.exists(file.path(basepath, "PLOTT", "TS", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT", "TSc")),
        dir.exists(file.path(basepath, "PLOTT", "TSc", "arkiv")),
        dir.exists(file.path(basepath, "PLOTT", "TL")),
        dir.exists(file.path(basepath, "PLOTT", "TL", "arkiv")))
  )
  expect_false(dir.exists(file.path(basepath, "NOTEXIST")))

  fs::dir_delete(testroot)
})

test_that("get_all_combinations works", {
  expect_no_error(get_all_combinations(cube1, c("KJONN", "ALDER")))
  expect_type(get_all_combinations(cube1, c("KJONN", "ALDER")), "list")
  expect_error(get_all_combinations(cube1, c("KJONN", "NOTEXIST")))
  expect_error(get_all_combinations(nocube, c("KJONN", "ALDER")))
})

test_that("aggregate_dimension", {
  expect_equal(find_total(cube1, "GEO"), 0)
  expect_equal(find_total(cube1, "ALDER"), "0_120")
  expect_error(aggregate_dimension(cube1, "AAR"), regexp = "cannot aggregate on 'AAR'")
  expect_no_error(aggregate_dimension(cube1, "GEO"))
})

test_that("convert_coltype works as expected", {
  df <- data.table::data.table(col1 = factor(c("0", "1", "2", "3")))
  expected <- data.table::data.table(col1 = c(0, 1, 2, 3))
  convert_coltype(df, "col1", "numeric")
  expect_equal(df, expected)

  df <- data.table::data.table(col1 = 1:3)
  expected <- data.table::data.table(col1 = as.character(1:3))
  convert_coltype(df, "col1", "character")
  expect_equal(df, expected)

  df <- data.table::data.table(col1 = 1:3)
  expected <- data.table::data.table(col1 = as.factor(1:3))
  convert_coltype(df, "col1", "factor")
  expect_equal(df, expected)
})
