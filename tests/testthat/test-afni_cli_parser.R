# Given a help topic, a parser is generated...

test_that("get help from a loaded function", {
  rd <- get_help_topic("example_func_for_parsing","afnistats")
  parsed <- Rd2list(rd)
  expected_args <- list("arg"="a","description"="Description of a")
  testthat::expect_equal(parsed[['arguments']][[2]],expected_args)
})

test_that("parse help from a loaded function", {
  parsed <- parse_help_topic("example_func_for_parsing","afnistats")
  expected_args <- list("arg"="a","description"="Description of a")
  testthat::expect_equal(parsed[['arguments']][[2]],expected_args)
})


test_that("parse help topic from an installed package", {
  parsed <- parse_help_topic("acf2AR","stats")

  # Check usage
  usage <- tail(capture.output(cat(parsed['usage'][[1]])),1)
  testthat::expect_equal(usage,"acf2AR(acf)")
  # Check arguments
  expected_args <- list(list("arg"="acf","description"="An autocorrelation or autocovariance sequence."))
  testthat::expect_equal(parsed[['arguments']],expected_args)
})




