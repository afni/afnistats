test_that("basic call to MBA without model fit works", {
  data_path <- system.file("extdata", "tiny_data.txt", package = "afnistats")
  model <- RBA(prefix = "result_from_rba", dataTable = data_path, do_not_fit_model = TRUE, ROI = "ROI1")
  expect_equal(model, NA)
  })

test_that("post process works with previously generated args", {
  load(system.file("extdata", "post_process_rba_args.RData", package = "afnistats"))
  output <- post_process_rba(fm, outFN, iterations, chains, EOIq, EOIc, qContr, ptm, ROI, PDP, model, dataTable)
  expect_equal(output,NULL)
})
