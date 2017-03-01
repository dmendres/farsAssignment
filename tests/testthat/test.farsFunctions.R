#fars tests
require(testthat)
testMakeFileName <- function() {
  expect_true(grepl("1999",make_filename(1999)))
  expect_false(grepl("2000", make_filename(1999)))
}
