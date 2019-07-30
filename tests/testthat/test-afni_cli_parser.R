# Given a help topic, a parser is generated...



test_that("get help from a loaded function", {
  rd <- get_help_topic("example_func_for_parsing","afnistats")
  parsed <- Rd2list(rd)
  expected_args <- list("arg"="a","description"="Description of a")
  testthat::expect_equal(parsed[['arguments']][[2]],expected_args)
})

test_that("parse help from a loaded function", {
  parsed <- parse_help_topic("example_func_for_parsing","afnistats")
  expected_args <- list("arg"="a","description"="Description of a")
  testthat::expect_equal(parsed[['arguments']][[2]],expected_args)
})


test_that("parse help topic from an installed package", {
  parsed <- parse_help_topic("acf2AR","stats")

  # Check usage
  usage <- tail(capture.output(cat(parsed['usage'][[1]])),1)
  testthat::expect_equal(usage,"acf2AR(acf)")
  # Check arguments
  expected_args <- list(list("arg"="acf","description"="An autocorrelation or autocovariance sequence."))
  testthat::expect_equal(parsed[['arguments']],expected_args)
})


test_that("default args can be missing, NA,or NULL but missing is converted to Not required",{
  sig_test <- function(pos_arg,null_default=NULL,na_default=NA,int_arg=3,logical_arg=TRUE,char_arg="a char"){}
  expected <- list(pos_arg="Not required",null_default=NULL,na_default=NA,int_arg=3,logical_arg=TRUE,char_arg="a char")
  expect_equal(get_arg_info_from_function_signature(sig_test), expected)

})




test_that("get_df_arg should produce a df with all cols being chr vectors except for default,required",{
  non_character_cols <- c('required','default')
  sig_test <- function(arg_1,arg_2=NULL){}
  parsed_help = list(alias=sig_test,arguments=list(list(arg="arg_1",description="arg_1 help"),list(arg="arg_2",description="arg_2 help")))
  expected <- tibble::tribble(
    ~dest,    ~help,         ~default,        ~required,
    "arg_1",  "arg_1 help",  "Not required",  TRUE,
    "arg_2",  "arg_2 help",   NULL,           FALSE,
  ) %>% dplyr::mutate_at(dplyr::vars(-non_character_cols),as.character)
  df_out <- get_df_arg(parsed_help)
  expect_identical(df_out, expected)


  expected <- tibble::tribble(
    ~dest,  ~metavar,   ~type,         ~nargs,  ~default,         ~required,   ~help,
    "x",    "STRING",   "character",   "1",     "Not required",   TRUE,         "Description for x",
    "a",    "N",        "integer",     "1",     NULL,             FALSE,        "Description of a",
    "b",    "N",        "integer",     "1",     NA,               FALSE,        "Description of b",
    "c",    "N",        "integer",     "+",     "a value",        FALSE,        "Description of c",
  )
  parsed_help <- parse_help_topic("example_func_for_parsing","afnistats")
  df_out <- get_df_arg(parsed_help)

  expect_equal_param_df(df_out, expected)

}
)

test_that("get_df_arg raises error if arguments in function signature are not documented in the roxygen block",{
  parsed_help <- parse_help_topic("example_func_for_parsing","afnistats")
  parsed_help$arguments[2] <- NULL
  missing_doc_error_txt <- 'For the function example_func_for_parsing, the
following arguments were in the function signature but were not documented as
arguments in the help: a. All arguments must be documented if you wish to
produce a command line interface.' %>% stringr::str_replace_all("\n",' ')
  expect_error(get_df_arg(parsed_help),missing_doc_error_txt)
}
)

test_that("run_tool works for an example function",{
  run_tool("example_func_for_parsing",package="afnistats")
})
