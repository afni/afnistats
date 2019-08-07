test_that("get_expected_level_combinations produces the right number of rows", {
  subj <- c("1", "2", "3", "4")
  roi1 <- c("w", "x", "y", "z")
  task_1 <- "staring at the screen"
  task_2 <- "cover the eyeballs"
  scanner <- "one big magnet"

  # fill out factor structure for an input dataset  a dataframe
  df <- tidyr::expand_grid(
    subj, roi1, task_1, task_2, scanner
  ) %>%
    dplyr::mutate(
      Y = runif(dplyr::n()),
      gender = runif(dplyr::n()) > 0.5,
      roi2 = roi1,
    )

  # For RBA
  rba_combination <-
    get_expected_level_combinations(
      df,
      experimental_unit = "subj",
      within_unit = c("task_1", "task_2", "scanner", "roi1"),
      pw_within_unit = NULL
    )
  expect_equal(
    dim(rba_combination)[1],
    prod(purrr::map_int(list(subj, task_1, task_2, scanner, roi1), length))
  )

  # For MBA
  mba_combination <- get_expected_level_combinations(
    df,
    experimental_unit = "subj",
    within_unit = c("task_1", "task_2", "scanner"),
    pw_within_unit = c("roi1", "roi2")
  )
  expect_equal(
    dim(mba_combination)[1],
    choose(length(roi1), 2) * prod(purrr::map_int(list(subj, task_1, task_2, scanner), length))
  )
})

test_that("get_expected_level_combinations evaluates variables in dataframe scope, not calling environment", {
  expect(FALSE, "need to check this")
})


# test_that("get_cols_key works", {
#   v <- c("Jasper", "Alice")
#   df <- expand.grid(v = v, w = v, stringsAsFactors = FALSE)
#   expected <- c("Jasper_Jasper", "Alice_Jasper", "Alice_Jasper", "Alice_Alice")
#   computed <- df %>%
#     dplyr::mutate(key = get_cols_key(c("v", "w"))) %>%
#     .$key
#   expect_equal(computed, expected)
#
#   # Doesn't fail for  factors
#   df <- expand.grid(v = v, w = v)
#   expected <- c("Jasper_Jasper", "Alice_Jasper", "Alice_Jasper", "Alice_Alice")
#   computed <- df %>%
#     dplyr::mutate(key = get_cols_key(c("v", "w"))) %>%
#     .$key
#   expect_equal(computed, expected)
# })
