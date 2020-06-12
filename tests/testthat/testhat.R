test_that("make_filename tests", {
  expect_error(object = make_filename(),info   = "missing arguments")
  expect_equal(object = make_filename(2013),expected = "accident_2013.csv.bz2",info = "make_filename oy the year argument")
  expect_equal(object = make_filename(2013:2014),expected = c("accident_2013.csv.bz2", "accident_2014.csv.bz2"),info = "make_filename of the years argument")
})

test_that("fars_read_years tests", {
  expect_error(object = fars_read_years(), info = "missing arguments")
})

test_that("fars_summarize_years tests", {
  expect_error(object = fars_summarize_years(), info = "missing arguments")
})

test_that("fars_map_state tests", {
  expect_error(object = fars_map_state(), info = "missing arguments")
})

