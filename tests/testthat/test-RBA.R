# example_model_output <- file.path(getwd(),'model_example.RData')

test_that("basic call to RBA with small data", {
  data_path <- system.file("extdata","tiny_data.txt",package = "afnistats")
  model <- RBA(prefix='result_from_rba',dataTable = data_path,ROI="ROI1")
  # expect_equal(model,NA)

  # Change to a temporary directory
  outdir <- tempfile()
  dir.create(outdir)
  old <- setwd(outdir)
  on.exit(setwd(old), add = TRUE)


  # Load output of MBA
  # load('result_from_mba.RData')
  # print(getwd())
  # print(dir())
  # testthat::expect_equal_to_reference(rs,example_model_output)

  # unlink(outdir,recursive=TRUE)
})
