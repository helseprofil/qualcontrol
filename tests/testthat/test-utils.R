invisible(capture.output(cube1 <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"))))
invisible(capture.output(cube2 <- read_cube(system.file("testdata", "testcube2.csv", package = "qualcontrol"))))
invisible(capture.output(readfiles(cube.new = "TRANGBODDHET_2023-11-20-11-05",
                                   modus.new = "KH",
                                   cube.old = "TRANGBODDHET_2023-01-03-14-38",
                                   modus.old = "KH",
                                   recode.old = T)))

test_that("identify_coltypes works", {

  expect_error(identify_coltypes(cube.new = NULL, cube.old = NULL),
               regexp = "cube.new must be provided")

  expect_type(identify_coltypes(cube1, cube2), "list")
  expect_length(identify_coltypes(cube1, cube2), 13)
  expect_length(identify_coltypes(cube1, NULL), 3)
})

test_that("get_cubename and cubedatetag works", {
  expect_equal(get_cubename(newcube), "TRANGBODDHET")
  expect_equal(get_cubedatetag(newcube), "2023-11-20-11-05")
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
        dir.exists(file.path(cubepath, "PLOTT", "TimeSeries_country", "arkiv")),
        dir.exists(file.path(cubepath, "PLOTT", "Diff_timetrends")),
        dir.exists(file.path(cubepath, "PLOTT", "Diff_timetrends", "arkiv")))
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
        dir.exists(file.path(cubepath2, "PLOTT", "TimeSeries_country", "arkiv")),
        dir.exists(file.path(cubepath2, "PLOTT", "Diff_timetrends")),
        dir.exists(file.path(cubepath2, "PLOTT", "Diff_timetrends", "arkiv")))
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

test_that("select_tellernevner works", {
  tellercolumns <- c("sumTELLER_uprikk", "sumTELLER", "TELLER_uprikk", "TELLER", "NOMATCH")
  nevnercolumns <- c("sumNEVNER_uprikk", "sumNEVNER", "NEVNER_uprikk", "NEVNER", "NOMATCH")

  expect_equal(select_teller_pri(tellercolumns), "sumTELLER_uprikk")
  expect_equal(select_teller_pri(tellercolumns[-1]), "sumTELLER")
  expect_equal(select_teller_pri(tellercolumns[-c(1:2)]), "TELLER_uprikk")
  expect_equal(select_teller_pri(tellercolumns[-c(1:3)]), "TELLER")
  expect_true(is.na(select_teller_pri(tellercolumns[-c(1:4)])))

  expect_equal(select_nevner_pri(nevnercolumns), "sumNEVNER_uprikk")
  expect_equal(select_nevner_pri(nevnercolumns[-1]), "sumNEVNER")
  expect_equal(select_nevner_pri(nevnercolumns[-c(1:2)]), "NEVNER_uprikk")
  expect_equal(select_nevner_pri(nevnercolumns[-c(1:3)]), "NEVNER")
  expect_true(is.na(select_nevner_pri(nevnercolumns[-c(1:4)])))
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

test_that("filter_cube works", {
  diminfo <- compare_dimensions(newcube, oldcube)
  expect_no_error(filter_cube(newcube, oldcube, diminfo, "new"))
  expect_no_error(filter_cube(newcube, oldcube, diminfo, "old"))
  expect_no_error(filter_cube(newcube, newcube, diminfo, "old"))

})

test_that("convert_coltype works as expected", {
  df <- data.table::data.table(col1 = factor(c("0", "1", "2", "3")))
  df2 <- data.table::data.table(col1 = c("0", "1", "2", "3"))
  expected <- data.table::data.table(col1 = c(0, 1, 2, 3))
  convert_coltype(df, "col1", "numeric")
  convert_coltype(df2, "col1", "numeric")
  expect_equal(df, expected)
  expect_equal(df2, expected)

  df <- data.table::data.table(col1 = 1:3)
  expected <- data.table::data.table(col1 = as.character(1:3))
  convert_coltype(df, "col1", "character")
  expect_equal(df, expected)

  df <- data.table::data.table(col1 = 1:3)
  expected <- data.table::data.table(col1 = as.factor(1:3))
  convert_coltype(df, "col1", "factor")
  expect_equal(df, expected)
})

test_that("split_kommuneniv, translate_geoniv and add_kommune works", {
  d <- split_kommuneniv(add_geoparams(data.table::data.table(GEO = c(0,3,0301,1103,4601,5001,1111,110301))))
  d2 <- split_kommuneniv(add_geoparams(data.table::data.table(GEO = c(110301, 1111, 5001, 4601, 1103, 0301, 3, 0))))
  expect_equal(levels(d$GEOniv), c("L", "F", "K", "k", "B"))
  expect_equal(levels(d2$GEOniv), levels(d$GEOniv))

  add_kommune(d)
  add_kommune(d2)

  expect_equal(d$KOMMUNE, c(NA, NA, "Oslo", "Stavanger", "Bergen", "Trondheim", NA, "Stavanger"))
  expect_equal(d2$KOMMUNE, c("Stavanger", NA, "Trondheim", "Bergen", "Stavanger", "Oslo", NA, NA))

  d <- translate_geoniv(d)
  expect_equal(as.character(d$GEOniv), c("Land", "Fylke", "Kommune", "Kommune", "Kommune", "Kommune", "Kommune", "Bydel"))
  d3 <- data.table::copy(d2)[, let(GEOniv = NULL)]
  expect_error(translate_geoniv(d3), regexp = "GEOniv column not present")

})

test_that("get_complete_strata works", {
  expect_no_error(get_complete_strata(cube1, by = c("GEO", "AAR", "KJONN", "ALDER"), type = "censored"))
  expect_equal(sum(cube1$n_censored), 3)
  cube1[, let(n_censored = NULL)]
  expect_no_error(get_complete_strata(cube1, by = c("AAR", "KJONN", "ALDER"), type = "censored"))
  expect_equal(sum(cube1$n_censored), 3)
  cube1[, let(n_censored = NULL)]
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

test_that("get_plotsavefolder works", {
  root <- file.path(getOption("qualcontrol.root"),getOption("qualcontrol.output"), getOption("qualcontrol.year"))

  expect_equal(get_plotsavefolder("KUBENAME", "Boxplot"), file.path(root, "KUBENAME", "PLOTT", "Boxplot"))
  expect_equal(get_plotsavefolder("KUBENAME", "TimeSeries_change"), file.path(root, "KUBENAME", "PLOTT", "TimeSeries_change"))
  expect_equal(get_plotsavefolder("KUBENAME", "TimeSeries_bydel"), file.path(root, "KUBENAME", "PLOTT", "TimeSeries_bydel"))
  expect_equal(get_plotsavefolder("KUBENAME", "TimeSeries_country"), file.path(root, "KUBENAME", "PLOTT", "TimeSeries_country"))
})

test_that("create_empty_standard_dt works", {
  expect_no_error(x <- create_empty_standard_dt())
  expect_equal(names(x), c(.standarddimensions, .standardvalues))
})

test_that("qc_round works", {
  expect_null(qc_round(NULL))
  d <- create_empty_standard_dt()
  addcols <- c("MIN", "change_MAX", "LOW", "HIGH", "wq25",
               "RATE.n", "SPVFLAGG", "SPVFLAGG_diff", "TELLER_diff", "TELLER_reldiff",
               "MEIS_new", "MEIS_old", "MEIS_diff", "MEIS_reldiff", "NO_ROUNDING")
  d[, (addcols) := NA_real_]
  target <- data.table::copy(d)
  d[, names(.SD) := as.numeric(1.234), .SDcols = names(d)[names(d) %notin% .validdims]]
  expect_no_error(d <- qc_round(d))

  target[, let(TELLER = 1.2,
               sumTELLER = 1.2,
               NEVNER = 1.2,
               sumNEVNER = 1.2,
               MEIS = 1.23,
               RATE = 1.23,
               SMR = 1.23,
               MIN = 1.23,
               change_MAX = 1.23,
               LOW = 1.23,
               HIGH = 1.23,
               wq25 = 1.23,
               RATE.n = 1,
               SPVFLAGG = 1,
               SPVFLAGG_diff = 1,
               TELLER_diff = 1.2,
               TELLER_reldiff = 1.23,
               MEIS_new = 1.23,
               MEIS_old = 1.23,
               MEIS_diff = 1.23,
               MEIS_reldiff = 1.23,
               NO_ROUNDING = 1.234)]

  expect_equal(d, target)
})
