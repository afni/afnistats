#' Analagous to expand.grid but removes symetrical or duplicated elements
#'
#' Computes the combinations of factors that are expected in the data to
#' construct a desired model. If a correlation matrix were constructed using the
#' factors as labels the result would be equivalent to the label combinations
#' required to access "upper.tri(corr_matrix)", i.e. the non symmetric
#' off-diagonal values. An additional complexity is for variables that are
#' expanded pairwise, effectively with themselves. An example would be region of
#' interest (ROI) for MBA. In this case the full set of factor levels should be
#' computed from both input columns to detect missing levels in one of the
#' columns.
#'
#' @param df An input dataframe.
#' @param experimental_unit A  string specifying the column in which unit is
#'   encoded. e.g. "Subject"
#' @param within_unit A string (single factor) or character vector (multiple) to
#'   specify columns for which each level should be crossed with other factors.
#' @param pw_within_unit A character vector specifying two columns, not included
#'   in "within_unit", that should have duplicate levels and should be expanded
#'   such that the half off-diagonal of their correlation matrix is used.
#'
#' @return A \code{\link[tibble]{tibble:::tbl_df}} with all factor levels
#'   crossed with each other. Permutations and duplications are removed for
#'   levels of the factor that is pairwise within unit (pw_within_unit).
#'
#' @export
#' @examples
#' subj <- c("1", "2")
#' roi <- c("frontal", "parietal")
#' df <- tidyr::expand_grid(subj, roi)
#' get_expected_level_combinations(df, "subj", "roi")
get_expected_level_combinations <- function(df, experimental_unit, within_unit = NULL, pw_within_unit = NULL) {

  #### Create symbols from the  string arguments
  unit_sym <- rlang::sym(experimental_unit)
  pw_within_unit_symlist <- rlang::syms(pw_within_unit)
  within_unit_symlist <- rlang::syms(within_unit)


  basic_levels <- df %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::select(!!unit_sym, !!!within_unit_symlist) %>%
    purrr::map(unique)

  levels_list <- df %>%
    dplyr::select(!!!pw_within_unit_symlist) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    rep(x = list(.), length(pw_within_unit)) %>%
    purrr::set_names(pw_within_unit) %>%
    append(basic_levels)

  df_out <- tidyr::crossing(!!!levels_list)

  if (!is.null(pw_within_unit)) {
    df_out <- df_out %>%
      dplyr::filter(!!pw_within_unit_symlist[[1]] < !!pw_within_unit_symlist[[2]])
  }
  df_out
}


#' Compute keys for a combination of factors
#'
#' This is no longer used as the functionality was incorporated into
#' get_expected_level_combinations. Given a list of equal length character
#' vectors refering to variables in the calling environment, this function sorts
#' and pastes them together taking one element at a time. Can be used with
#' dplyr::mutate.
#'
#' @param cols A list containing equal length vectors
#'
#' @return A flattened list with a length equal to that of each individual
#'   element of the input.
#'
#' @examples
#' x <- c("a", "b", "c")
#' y <- c("1", "2", "3")
#' get_cols_key(list("x", "y"))
#' iris %>%
#'   head(5) %>%
#'   dplyr::mutate(key = get_cols_key(c("Sepal.Width", "Sepal.Length")))
get_cols_key <- function(cols) {

  # this function is only needed here, as in not needed
  merge_sort <- function(cols_values) {
    purrr::pmap_chr(
      cols_values,
      ~ stringr::str_c(stringr::str_sort(c(...)), collapse = "_")
    )
  }

  # column names need to be evaluated using the dataframe as an environment
  cols_quosure <- rlang::enquo(cols)
  cols_syms <- rlang::syms(cols)

  cols_values <- purrr::map(
    cols_syms,
    ~ rlang::eval_tidy(.x, rlang::quo_get_env(cols_quosure))
  ) %>% purrr::map(as.character)

  merge_sort(cols_values)
}
