test_that("basic call to MBA without model fit works", {
  data_path <- system.file("extdata", "tiny_data.txt", package = "afnistats")
  model <- RBA(prefix = "result_from_rba", dataTable = data_path, do_not_fit_model = TRUE)
  expect_equal(model, NA)
})
