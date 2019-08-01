example_model_output <- file.path(getwd(), "model_example.RData")

test_that("basic call to MBA without model fit works", {
  run_fast <- TRUE
  data_path <- system.file("extdata", "tiny_data.txt", package = "afnistats")
  if (!run_fast) cat("Running MBA, this will take some time...")
  model <- MBA(prefix = "result_from_mba", dataTable = data_path, do_not_fit_model = run_fast)
  expect_equal(model, NA)
})
