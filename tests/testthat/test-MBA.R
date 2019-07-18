test_that("basic call to MBA with small data", {
  outdir <- tempfile()
  print(outdir)
  dir.create(outdir)
  setwd(outdir)
  data_path <- system.file("extdata","tiny_data.txt",package = "afnistats")
  MBA(dataTable = data_path,chains = 2,iterations = 100)
  unlink(outdir,recursive=TRUE)
})


