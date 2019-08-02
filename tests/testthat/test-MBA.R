example_model_output <- file.path(getwd(), "model_example.RData")

test_that("basic call to MBA without model fit works", {
  data_path <- system.file("extdata", "tiny_data.txt", package = "afnistats")
  model <- MBA(prefix = "result_from_mba", dataTable = data_path, do_not_fit_model = TRUE)
  expect_equal(model, NA)
})
