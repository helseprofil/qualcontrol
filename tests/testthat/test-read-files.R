test_that("readfiles_checkargs", {

  cube.new_correct <- cube.old_correct <- "CUBE_1234-12-34-56-78"
  cube.new_correct_csv <- cube.old_correct_csv <- "CUBE_1234-12-34-56-78.csv"
  cube.new_incorrect <- cube.old_incorrect <- "CUBE_1234-12-34-56"
  cube.new_null <- NULL
  cube.old_null <- NULL
  modus_correct = "KH"
  modus_incorrect = "KHS"
  recode_correct = TRUE
  recode_incorrect = NULL

  expect_no_error(readfiles_checkargs(cube.new_correct, cube.old_correct, modus_correct,  modus_correct, recode_correct, recode_correct))
  expect_no_error(readfiles_checkargs(cube.new_correct_csv, cube.old_correct_csv, modus_correct, modus_correct, recode_correct, recode_correct))
  expect_no_error(readfiles_checkargs(cube.new_correct_csv, cube.old_null, modus_correct, modus_correct, recode_correct, recode_correct))

  expect_error(readfiles_checkargs(cube.new_null, cube.old_correct_csv, modus_correct, modus_correct, recode_correct, recode_correct))
  expect_error(readfiles_checkargs(cube.new_incorrect, cube.old_correct, modus_correct, modus_correct, recode_correct, recode_correct))
  expect_error(readfiles_checkargs(cube.new_correct, cube.old_incorrect, modus_correct, modus_correct, recode_correct, recode_correct))
  expect_error(readfiles_checkargs(cube.new_correct, cube.old_correct, modus_incorrect, modus_correct, recode_correct, recode_correct))
  expect_error(readfiles_checkargs(cube.new_correct, cube.old_correct, modus_correct, modus_incorrect, recode_correct, recode_correct))
  expect_error(readfiles_checkargs(cube.new_correct, cube.old_correct, modus_correct, modus_correct, recode_incorrect, recode_correct))
  expect_error(readfiles_checkargs(cube.new_correct, cube.old_correct, modus_correct, modus_correct, recode_correct, recode_incorrect))
})

test_that("find_cube", {
  expect_null(find_cube(cubename = NULL))
  expect_error(find_cube(cubename = "NOFILE_1234-12-34-56-78", cubemodus = "KH"))
})

test_that("read_cube renames columns", {
  invisible(capture.output(rename <- read_cube(system.file("testdata", "rename.csv", package = "qualcontrol"), type = "New")))
  invisible(capture.output(rename2 <- read_cube(system.file("testdata", "rename2.csv", package = "qualcontrol"), type = "Old")))
  correctnames = c("TELLER", "RATE", "MEIS", "sumTELLER", "sumNEVNER", "SMR", "UTDANN")

  expect_identical(names(rename), correctnames)
  expect_identical(names(rename2), correctnames)
  expect_no_error(invisible(capture.output(list_renamecols(attributes(rename)$colnameinfo$orgnames,
                                                           attributes(rename)$colnameinfo$newnames,
                                                           type = "Test"))))
  expect_no_error(invisible(capture.output(is_valid_outcols(rename))))
  expect_output(is_valid_outcols(rename), "NB! New file contains TELLER, sumTELLER, sumNEVNER. Is this ok for ALLVIS?")
})

test_that("recode_geo works", {
  invisible(capture.output(cube <- read_cube(system.file("testdata", "testcube1.csv", package = "qualcontrol"), type = "New")))
  expect_false(all(cube$GEO %in% .validgeo))
  invisible(capture.output(cube_recoded <- recode_geo(cube, recode = TRUE)))
  expect_true(all(cube_recoded$GEO %in% .validgeo))
})

