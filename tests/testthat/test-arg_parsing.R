load(system.file("extdata", "example_args_1.rda", package = "afnistats"))
load(system.file("extdata", "example_parsed_args_1.rda", package = "afnistats"))



test_that("user arg parsing using old function", {
  expect_equal(example_parsed_args_1, parse_args(example_args_1))
})

test_that("user arg parsing for MBA from roxygen-generated parser", {
  parser <- create_parser_from_function("MBA","afnistats")
  fname <- file.path(system.file('extdata',package="afnistats"),'parser_reference.RData')
  expect_equal_to_reference(parser,fname)
  # parser$print_help()

  parsed_args <- parser$parse_args(example_args_1)
  expect_equal(example_parsed_args_1$ROI1, parsed_args$ROI1)
})
