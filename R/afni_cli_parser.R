#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


parser_error <- function(c){
  msg <- conditionMessage(c)
  error_pat <- "parse error:|help requested:"
  if (msg %>% stringr::str_detect(error_pat)){
    cat(msg %>% stringr::str_split(error_pat) %>% purrr::map_chr(2))
  }
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' Run a tool
#' @export
run_tool <- function(toolname,package="afnistats",catch_error=TRUE,user_args=commandArgs(trailingOnly = TRUE)){

  parser <- create_parser_from_function(toolname,package)
  tryCatch(parsed_ops <- parser$parse_args(user_args),
           error = function(c){if (catch_error) parser_error(c) else{stop(c)}},
           quietly = TRUE
  )

  do.call(toolname,parsed_ops)
}

#'Generate a command line parser from function documentation
#'
#'If a section "CLI info" is contained in the help text for a function, this
#'tool parses the information in order to create a command line parser. The
#'formatting for the section should be input to \code{\link[tibble]{tribble}}
#'(all characters preceding the first '~' are discarded). The output is a
#'command-line parser that can process user arguments obtained from
#'\code{commandArgs(trailing=TRUE)}. Positional arguments in the function become
#'required arguments on the command line. Keyword arguments become long options
#'with the same default as the R function.
#'
#'The column headings represent values that are passed to the add_argument
#'method of \code{\link[argparse]{argparse}} and it must contain at least:
#'\enumerate{ \item param, the argument name. This becomes the argument/option
#'name in the command line tool \item nargs, the number of items expected to
#'follow the parameter. \item metavar, a representation of the arguments input.
#'This helps cue the user to what information they should provide. It is not
#'required when nargs is 0 \item type, the expected type of the input.}
#'
#'Other columns that might be useful \enumerate{\item group_inc, space separated
#'group labels to which the parameter belongs. All parameters in a group must be
#'provided together \item group_ex, same as group_inc but denotes that
#'parameters are mutually exclusive in their usage \item prefix, an alternative
#'string to denote options if you wish to make your interface deviate from the
#''--' of the conventional
#'\href{https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html}{
#'GNU/LINUX interface}. \item other arguments to
#'argparse.ArgumentParser$add_argument.}
#'
#'
#'
#'@param topic The help topic that should be parsed
#'@param package The package in which the help topic is contained
#'
#'@return argparse:ArgumentParser
#'@export
create_parser_from_function <- function(topic="MBA",package="afnistats"){
  parsed_help <- parse_help_topic(topic,package)
  parser <- create_parser_from_help(parsed_help)
  parser
}



create_parser_from_help <- function(parsed_help){

  parser <- argparse::ArgumentParser(description=fix_help_string(parsed_help$description),add_help=FALSE)
  parser$add_argument('-help',action='help') # add AFNI's help flag
  df_arg <- get_df_arg(parsed_help)
  df_arg %<>%
    dplyr::group_by(1:dplyr::n()) %>%
    tidyr::nest()
  df_arg %>%
    dplyr::mutate(parsed = purrr::map(data, ~ add_arg(.x,parser)))
  parser

}

add_arg <- function(df,parser){
  # nargs can be a character
  if (!df$nargs %in% c("?","+","*")){
    df$nargs <- as.integer(df$nargs)
    # option flags that take no arguments are treated differently.
    if  (df$nargs == 0){
    df <- df %>%
      dplyr::select(-c(nargs,type,metavar,default)) %>%
      dplyr::mutate(action = 'store_true')
    }
    # required arguments can't have a default
    if (df$required == TRUE){
      df <- dplyr::select(df,-c(default))
    }
  }


  # "Not required" is used to specify that the value should not be used to create the parser and is dropped accordingly
  arg_list <- df %>%  dplyr::select_if(~ !is.character(.x) || stringr::str_detect(.x,"^Not required$",negate=TRUE))
  arg_list <- arg_list %>% purrr::transpose() %>% tibble::deframe()
  # arg_list <- df %>% purrr::transpose() %>% purrr::map(~purrr::discard(.x,is.na))
  do.call(parser$add_argument,c(stringr::str_c('-',df$dest),arg_list))
  # This can be useful for debugging...
  help_val <- capture.output(parser$print_help())
  if (sum(stringr::str_detect(help_val,'.{1,15}Error'))) {
    stop(stringr::str_c("parser broken for: ",arg_list$dest))
  }

  TRUE
}

fix_help_string <- function(string){
  stringr::str_replace_all(string,'\\n',' ')
}


parse_cli_info_str <- function(parsed_help){
  help_sections <-  parsed_help[names(parsed_help) == "section"]
  if (!length(help_sections)){return(tibble::tibble(dest=character()))}
  tribble_str <- help_sections %>%
    purrr::keep(~ stringr::str_detect(.x,stringr::regex("cli ?info", ignore_case = TRUE))) %>%
    stringr::str_split('~',n=2) %>%
    purrr::flatten() %>%
    .[[2]]

  tribble_str %>%
    stringr::str_replace_all("~",'') %>%
    stringr::str_replace("\\n","empty\n") %>%
    readr::read_csv() %>%
    dplyr::select(-empty)
}


get_arg_info_from_function_signature <- function(funcname){
  formals(funcname) %>%
  purrr::map(~ rlang::maybe_missing(.x,"Not required"))  # the 'missing' symbol is nasty, get rid of it
}

get_df_arg_new <- function(parsed_help){
  arguments <- purrr::flatten(parsed_help['arguments'])
  help_vals <- purrr::map_chr(arguments,'description') %>%
    purrr::set_names(purrr::map(arguments,'arg'))

  # Parse the cli info section for argparse arguments
  df <- parse_cli_info_str(parsed_help)

  # parse the function signature for required arguments and defaults
  default_args <- get_arg_info_from_function_signature(parsed_help)

    # dplyr::mutate_all(as.character)

  # Construct full output
  df %>%
    dplyr::mutate(
      help= help_vals[dest],
      nargs = fix_nargs(nargs),
      default = default_args[dest], # Get the default values for the arguments

      required = ifelse(is.na(default),TRUE,FALSE),
    )
}

get_df_arg <- function(parsed_help){
  arguments <- purrr::flatten(parsed_help['arguments'])
  cli_info <- parse_cli_info_str(parsed_help )
  arg_defaults <- formals(parsed_help$alias)

  df_init <- purrr::map(arguments,'description') %>%
    purrr::set_names(purrr::map(arguments,'arg')) %>%
    tibble::enframe(name="dest",value="help") %>%
    dplyr::left_join(cli_info,by="dest")

  check_func_sig_and_doc_args_match(parsed_help,df_init,arg_defaults)
  df_arg <- df_init %>%
    dplyr::mutate(
      help = fix_help_string(help),
      default = unname(arg_defaults[dest]), # Get the default values for the arguments
      default = purrr::map(default,~ rlang::maybe_missing(.x,"Not required")), # the 'missing' symbol is nasty, get rid of it
      required = dplyr::if_else(suppressWarnings(stringr::str_detect(default,"Not required")),TRUE,FALSE)
    )

  df_arg
}

check_func_sig_and_doc_args_match <- function(parsed_help,df_init,arg_defaults){
  x <- names(arg_defaults)
  y <- df_init$dest
  arg_diff <- c(setdiff(x,y),setdiff(y,x))
  undocumented_args_error <- paste(
    c('For the function ',
      parsed_help$alias,
      ', the following arguments were in the function signature but were not documented as ',
      'arguments in the help: ',
      arg_diff,
      '. All arguments must be documented if you wish to produce a command line interface.'),
  collapse = '')
  if (length(arg_diff)) stop(undocumented_args_error)
  }

#' parse_help_topic
#'
#' For a given function the help text is retrieved and returned as a list.
#'
#' @param topic Function for which to get help
#' @param package_name Package from which the function comes
#'
#' @return Parsed help as a list
parse_help_topic <- function(topic,package_name){
  help_rd <- get_help_topic(topic,package_name)
  help_val <- Rd2list(help_rd)
  help_val
}

#' An example function for testing parsing to generate a cli
#'
#' Description
#'
#' Details
#'
#' @param x Description for x
#' @param a Description of a
#' @param b
#'   Description of b
#' @param c Description of c
#' @section Important:
#' Don't run with scissors!
#' @section Cli info:
#' See \code{\link{create_parser_from_function}} for details on this section.
#' \preformatted{
#' ~dest,  ~metavar,   ~type,         ~nargs,
#' "x",    "STRING",   "character",   "1",
#' "a",    "N",        "integer",     "1",
#' "b",    "N",        "integer",     "1",
#' "c",    "N",        "integer",     "+",
#' }
#' @export
example_func_for_parsing <- function(x,a=NULL,b=NA,c="a value"){
  print("sample")
}


Rd2list <- function(Rd){
  #from https://stackoverflow.com/questions/8918753/r-help-page-as-object
  names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"),2)
  temp_args <- Rd$arguments

  Rd$arguments <- NULL
  myrd <- lapply(Rd, unlist)
  myrd <- lapply(myrd, paste, collapse="")

  temp_args <- temp_args[sapply(temp_args , attr, "Rd_tag") == "\\item"]
  temp_args <- lapply(temp_args, lapply, paste, collapse="")
  temp_args <- lapply(temp_args, "names<-", c("arg", "description"))
  myrd$arguments <- temp_args
  return(myrd)
}

#' get_help_topic
#'
#'First devtools installed packages are checked. If this fails, installed packages are checked
#'
#' @param topic Function for which to get help
#' @param package_name Package from which the function comes
#'
#' @return Rd format help. Can be parsed with tools::Rd2text and other similar functions
get_help_topic <- function(topic,package_name){
  help_rd <- tryCatch(get_dev_help(topic,package_name),
                      error = function(e) get_installed_help(topic,package_name),
                      quietly =TRUE
  )
  help_rd
}

get_installed_help <- function(topic,package_name){
  help_file <- utils::help(topic,package=eval(package_name))
  rd <- utils:::.getHelpFile(help_file)
  rd
}

get_dev_help <- function(topic,package_name){

  if (requireNamespace("pkgload", quietly = TRUE)) {
    help_file <- pkgload::dev_help(topic,package_name)
    # below fails because it is not rdx
    # rd <- utils:::.getHelpFile(help_file$path)
    rd <- tools::parse_Rd(help_file$path)
    rd
    }
}

expect_equal_param_df <- function(df1,df2){
  expect_equal(df1$default,df2$default)
  expect_equal(
    df1 %>% dplyr::select(-default),
    df2 %>% dplyr::select(-default)
  )
}
