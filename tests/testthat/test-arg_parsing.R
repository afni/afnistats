# All "interface functions" should have help that can be used to construct a valid command line parsing object...


# test_that("user arg parsing using old function", {
#   expect_equal(example_parsed_args_1, parse_args(example_args_1))
# })

test_that("user arg parsing for MBA from roxygen-generated parser", {
  mba_parser <- create_parser_from_function("MBA","afnistats")

  # Minimal args
  expect_equal(
    mba_parser$parse_args(c('-dataTable','inst/extdata/tiny_data.txt')),
    list(EOI = "Intercept",MD = FALSE,ROI1 = "ROI1",ROI2 = "ROI2",Subj = "Subj",
         Y = "Y",cVars = NULL,chains = 4,dataTable = "inst/extdata/tiny_data.txt",
         dbgArgs = FALSE,do_not_fit_model = FALSE,iterations = 1000,model = 1,
         prefix = "result",qContr = NULL,qVars = "Intercept",r2z = FALSE,stdz = NULL,verb = 0)
  )


  # some basic args
  mba_args_1 <- readRDS(system.file("extdata", "mba_args_1.RData", package = "afnistats"))
  mba_ref_file_1 <- system.file("extdata", "mba_parsed_args_1.RData", package = "afnistats")
  expect_equal_to_reference(
    mba_parser$parse_args(mba_args_1) %>% .[sort(names(.))],
    mba_ref_file_1
    )

  })

test_that("user arg parsing for RBA from roxygen-generated parser", {
  rba_parser <- create_parser_from_function("RBA","afnistats")

  # Minimal args
  expect_equal(
    rba_parser$parse_args(c('-dataTable','inst/extdata/tiny_data.txt','-prefix','output')),
    list(EOI = "Intercept", MD = FALSE, PDP = NULL, ROI = "ROI", Subj = "Subj", Y = "Y",
         cVars = NULL, chains = 1, dataTable = "inst/extdata/tiny_data.txt", dbgArgs = FALSE,
         do_not_fit_model = FALSE, iterations = 1000, model = 1, prefix = "output", qContr = NULL,
         qVars = "Intercept", r2z = FALSE, stdz = NULL, verb = 0)
  )




  rba_parser$parse_args(c( "-dataTable" , "inst/extdata/tiny_data.txt", "-prefix" , "output","-PDP" , "4", "4"))
  # Basic args
  # rba_args_1 <- readRDS(system.file("extdata", "rba_args_1.RData", package = "afnistats"))
  # rba_ref_file_1 <- system.file("extdata", "rba_parsed_args_1.RData", package = "afnistats")
  # parsed_args <- parser$parse_args(rba_args_1) %>% .[sort(names(.))]
  # # expect_equal_to_reference(parsed_args,rba_ref_file_1)
})

test_that("arg parsing works for an installed executable",{
  # NOTE: this will run old code if the package has not been re-installed
  # and will fail if it is not installed but loaded into the environment
  #  using devtools


  skip_if_not(
    as.logical(length(find.package("afnistats",.libPaths(),quiet=TRUE))),
    message = "package installed")

  data_path <- system.file("extdata","tiny_data.txt",package = "afnistats")
  err_code <- withr::with_path(
    system.file('exec',package="afnistats"),
    system(stringr::str_c("RBA -prefix result_3 -dataTable ",data_path,"  -ROI ROI1")),
    action = "prefix")
  expect(err_code == 0,failure_message = "RBA executable is not working")

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
