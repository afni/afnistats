test_that("get_nr works", {

  col_names <- c("cat_var_1","cat_var_2")

  df <- tibble::tribble(
    ~col_1, ~cat_var_1,      ~cat_var_2,
    "text", "unique_val_1",   "unique_val_1",
    "text", "unique_val_2",   "unique_val_1",
    "text", "unique_val_1",   "unique_val_3",
    "text", "unique_val_1",   "unique_val_4",
  )

  # Should work with a one label or a vector of labels
  expect_equal(
    get_nr(df,col_names[1]),
    2
  )

  expect_equal(get_nr(df,col_names),4)

  # Should work with regular dataframe
  expect_equal(get_nr(data.frame(df),col_names),4)

  # Should work if columns are factors
  expect_equal(
    get_nr(df %>% dplyr::mutate(cat_var_1 = factor(cat_var_1)),col_names),
    4
    )


})
