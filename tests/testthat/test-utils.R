invisible(capture.output(cube1 <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
invisible(capture.output(cube2 <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))
invisible(capture.output(readfiles(cube.new = "LUFT_2_5_PWC_2024-02-28-20-41")))

test_that("identify_coltypes works", {

  expect_error(identify_coltypes(cube.new = NULL, cube.old = NULL),
               regexp = "cube.new must be provided")

  expect_type(identify_coltypes(cube1, cube2), "list")
  expect_length(identify_coltypes(cube1, cube2), 13)
  expect_length(identify_coltypes(cube1, NULL), 3)
})

test_that("get_cubename and cubedatetag works", {
  expect_equal(get_cubename(newcube), "LUFT_2_5_PWC")
  expect_equal(get_cubedatetag(newcube), "2024-02-28-20-41")
})

test_that("generate_qcfolders works", {
  skip_if(getOption("qualcontrol.skipslowtest"), "Skipping generate_qcfolders-test")
  orgyear <- getOption("qualcontrol.year")

  generate_qcfolders(cube = "TESTKUBENAVN", year = "TESTFOLDER")
  testroot <- file.path(getOption("qualcontrol.root"),
                        getOption("qualcontrol.output"),
                        "TESTFOLDER")

  cubepath <- file.path(testroot, "TESTKUBENAVN")
  expect_true(
    all(dir.exists(cubepath),
        dir.exists(file.path(cubepath, "arkiv")),
        dir.exists(file.path(cubepath, "FILDUMPER")),
        dir.exists(file.path(cubepath, "FILDUMPER", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT")),
        dir.exists(file.path(cubepath, "PLOTT", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "Boxplot")),
        dir.exists(file.path(cubepath, "PLOTT", "Boxplot", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "Boxplot_change")),
        dir.exists(file.path(cubepath, "PLOTT", "Boxplot_change", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_change")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_change", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_bydel")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_bydel", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_country")),
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_country", "arkiv")))
  )
  expect_false(dir.exists(file.path(cubepath, "NOTEXIST")))

  options(qualcontrol.year = 9999)
  generate_qcfolders(cube = "TESTKUBENAVN", year = NULL)
  testroot2 <- file.path(getOption("qualcontrol.root"),
                        getOption("qualcontrol.output"),
                        "9999")

  cubepath2 <- file.path(testroot2, "TESTKUBENAVN")
  expect_true(
    all(dir.exists(cubepath2),
        dir.exists(file.path(cubepath2, "arkiv")),
        dir.exists(file.path(cubepath2, "FILDUMPER")),
        dir.exists(file.path(cubepath2, "FILDUMPER", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT")),
        dir.exists(file.path(cubepath2, "PLOTT", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "Boxplot")),
        dir.exists(file.path(cubepath2, "PLOTT", "Boxplot", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "Boxplot_change")),
        dir.exists(file.path(cubepath2, "PLOTT", "Boxplot_change", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_change")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_change", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_bydel")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_bydel", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_country")),
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_country", "arkiv")))
  )
  expect_false(dir.exists(file.path(cubepath2, "NOTEXIST")))


  fs::dir_delete(testroot)
  fs::dir_delete(testroot2)
  options(qualcontrol.year = orgyear)
})

test_that("get_all_combinations works", {
  expect_no_error(get_all_combinations(cube1, c("KJONN", "ALDER")))
  expect_type(get_all_combinations(cube1, c("KJONN", "ALDER")), "list")
  expect_error(get_all_combinations(cube1, c("KJONN", "NOTEXIST")))
  expect_error(get_all_combinations(nocube, c("KJONN", "ALDER")))
})

test_that("finn_total and aggregate_cube works", {
  expect_equal(find_total(cube1, "GEO"), 0)
  expect_true(is.na(find_total(cube1[GEO != 0], "GEO")))
  expect_equal(find_total(cube1, "ALDER"), "0_120")
  expect_true(is.na(find_total(cube1[ALDER != "0_120"], "ALDER")))

  expect_error(aggregate_cube(cube1, "AAR"), regexp = "cannot aggregate on 'AAR'")
  expect_no_error(aggregate_cube(cube1, "GEO"))
  expect_no_error(test <- aggregate_cube(cube1[ALDER != "0_120"], "ALDER"))
  expect_equal(unique(test$ALDER), "Total")

  expect_no_error(aggregate_cube_multi(cube1, c("GEO", "ALDER", "UTDANN", "INNVAND")))
})

# test_that("filter_cube works", {
#
# })

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

test_that("add_kommune works", {
  d <- data.table::data.table(GEO = c(0,3,301,1103, 4601,5001, 1806, 1508, 110301, 30105, 460106, 500104))
  add_geoparams(d)
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
