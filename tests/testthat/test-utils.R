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

test_that("aggregate_cube works", {
  expect_equal(find_total(cube1, "GEO"), 0)
  expect_equal(find_total(cube1, "ALDER"), "0_120")
  expect_error(aggregate_cube(cube1, "AAR"), regexp = "cannot aggregate on 'AAR'")
  expect_no_error(aggregate_cube(cube1, "GEO"))
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

test_that("add_geoniv works", {
  d1 <- data.table::data.table(GEO = c(0,3,1101,1111,110301))
  d2 <- data.table::copy(d1)
  d3 <- data.table:::copy(d1)[, .SD[c(1,3,5)]]
  add_geoniv(d1, combine.kommune = F)
  add_geoniv(d2, combine.kommune = T)
  add_geoniv(d3)
  expect_equal(levels(d1$GEOniv), c("L", "F", "K", "k", "B"))
  expect_equal(levels(d2$GEOniv), c("L", "F", "K", "B"))
  expect_equal(levels(d3$GEOniv), c("L", "K", "B"))
})

test_that("add_kommune works", {
  d <- data.table::data.table(GEO = c(0,3,301,1103, 4601,5001, 1806, 1508, 110301, 30105, 460106, 500104))
  add_geoniv(d, combine.kommune = T)
  add_kommune(d)
  expect_equal(d$KOMMUNE, c(NA, NA, "Oslo", "Stavanger", "Bergen", "Trondheim", NA, NA,"Stavanger", "Oslo", "Bergen", "Trondheim"))
})

test_that("get_complete_strata works", {
  expect_no_error(get_complete_strata(cube1, by = c("AAR", "KJONN", "ALDER"), type = "censored"))
  expect_equal(sum(cube1$n_censored), 3)
  expect_no_error(get_complete_strata(cube1, by = c("AAR", "KJONN", "ALDER"), type = "missing", valuecolumn = "TELLER"))
  expect_equal(sum(cube1$n_censored), 0)
  cube1[, let(n_censored = NULL)]
  expect_error(get_complete_strata(cube1, by = c("AAR", "KJONN", "ALDER"), type = "missing", valuecolumn = NULL),
               regexp = "When type = 'missing', a valid column name must be provided to the value argument")
})

test_that("update_qcyear works", {

  configyear <- yaml::yaml.load_file(paste("https://raw.githubusercontent.com/helseprofil/config/main/config-qualcontrol.yml"))$year
  update_qcyear(year = 9999)
  expect_equal(getOption("qualcontrol.year"), 9999)
  update_qcyear()
  expect_equal(getOption("qualcontrol.year"), configyear)
})
