# All "interface functions" should have help that can be used to construct a valid command line parsing object...


load(system.file("extdata", "example_args_1.rda", package = "afnistats"))
load(system.file("extdata", "example_parsed_args_1.rda", package = "afnistats"))



# test_that("user arg parsing using old function", {
#   expect_equal(example_parsed_args_1, parse_args(example_args_1))
# })

test_that("user arg parsing for MBA from roxygen-generated parser", {
  parser <- create_parser_from_function("MBA","afnistats")


  parsed_args <- parser$parse_args(example_args_1)
  example_parsed_args_1 %>%  tibble::enframe() %>% dplyr::select(dplyr::vars(-help))

  expect_equal(example_parsed_args_1$ROI1, parsed_args$ROI1)
})

#
# test_that("old MBA with small data", {
#   outdir <- tempfile()
#   dir.create(outdir)
#   old <- setwd(outdir)
#   on.exit(setwd(old), add = TRUE)
#
#   data_path <- system.file("extdata","tiny_data.txt",package = "afnistats")
#   withr::with_path(
#     "/Users/rodgersleejg/abin/",
#     system(stringr::str_c("MBA -prefix result_3 -dataTable ",data_path)),
#     action = "prefix")
#
#   #
#   #   testthat::expect_equal_to_reference()
#   #
#   #   unlink(outdir,recursive=TRUE)
# })
# #
# #
