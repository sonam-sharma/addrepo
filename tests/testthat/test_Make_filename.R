library(mynewpackage)
context("Check whether mynewpackage functions are working correctly")
test_that("make filename function check",{
  expect_that(make_filename(2013),equals("accident_2013.csv"))
  expect_equal(make_filename(2014),equals("accident_2014.csv"))
}
)
test_that("checking far read function",{

  expect_silent(fars_read("accident_2013.csv"))
  expect_error(fars_read("accident_2016.csv"))
})
