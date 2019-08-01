test_that("basic call to MBA without model fit works", {
  run_fast <- FALSE
  data_path <- system.file("extdata", "tiny_data.txt", package = "afnistats")
  if (!run_fast) cat("Running RBA, this will take some time...")
  model <- RBA(prefix = "result_from_rba", dataTable = data_path, do_not_fit_model = run_fast,ROI='ROI1')
  expect_equal(model, NA)
})

test_that("post process works with previously generated args", {
  load(system.file("extdata","post_process_rba_args.RData",package="afnistats"))
  post_process_rba(fm, outFN, iterations, chains, EOIq, EOIc, qContr, ptm, nR, ROI,PDP,model,dataTable)

  })
