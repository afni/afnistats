#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


parse_error <- function(c){
  msg <- conditionMessage(c)
  if (msg %>% stringr::str_detect("parse error:")){
    cat(msg %>% stringr::str_split("parse error:") %>% purrr::map_chr(2))
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
           error = function(c){if (catch_error) parse_error(c) else{stop(c)}},
           quietly = TRUE
  )

  do.call(toolname,parsed_ops)
}

create_parser_from_function <- function(topic="MBA",package="afnistats"){
  parsed_help <- parse_help_topic(topic,package)
  parser <- create_parser_from_help(parsed_help)
  parser
}

create_parser_from_help <- function(parsed_help){

  parser <- argparse::ArgumentParser(description=fix_help_string(parsed_help$description),add_help=FALSE)
  parser$add_argument('-help','-h',action='help') # add AFNI's help flag
  df_arg <- get_df_arg(parsed_help)
  df_arg %>%
    dplyr::group_by(1:dplyr::n()) %>%
    tidyr::nest() %>%
    dplyr::mutate(parsed = purrr::map(data, ~ add_arg(.x,parser)))
  parser

}

add_arg <- function(df,parser){
  # option flags that take no arguments are treated differently. nargs can be a character, hence the check for NA
  if (!suppressWarnings(is.na(df$nargs == 0)) && df$nargs == 0){
    df <- df %>%
      dplyr::select(-c(nargs,type,metavar,default)) %>%
      dplyr::mutate(action = 'store_true')
  }
  # "Not required" is used to specify that the value should not be used to create the parser and is dropped accordingly
  arg_list <- df %>%  dplyr::select_if(~ !is.character(.x) || stringr::str_detect(.x,"Not required",negate=TRUE))  %>% purrr::transpose() %>% tibble::deframe()
  do.call(parser$add_argument,c(stringr::str_c('-',df$dest),arg_list))
  help_val <- capture.output(parser$print_help())
  if (sum(stringr::str_detect(help_val,'raise ValueError|NameError'))) {
    stop(stringr::str_c("parser broken for: ",arg_list$dest))
  }

  TRUE
}

fix_help_string <- function(string){
  stringr::str_replace_all(string,'\\n',' ')
}

get_df_arg <- function(parsed_help){
  arguments <- purrr::flatten(parsed_help['arguments'])
  df_arg <- purrr::map(arguments,'description') %>%
    purrr::set_names(purrr::map(arguments,'arg')) %>%
    tibble::enframe() %>%
    dplyr::rename(dest = name) %>%
    tidyr::separate(value,sep='\\(cli_info: ',c('help','metadata')) %>%
    tidyr::separate('metadata',sep=',',c('metavar','type','nargs')) %>%
    dplyr::mutate(
      nargs = fix_nargs(nargs),
      help = fix_help_string(help),
      default = formals(parsed_help$alias)[dest], # Get the default values for the arguments
      default = purrr::map(default,~ rlang::maybe_missing(.x,"Not required")), # the 'missing' symbol is nasty, get rid of it
      required = ifelse(suppressWarnings(stringr::str_detect(default,"Not required")),TRUE,"Not required")
      )

  df_arg
}

fix_nargs <- function(nargs){
  nargs %>%
    stringr::str_replace('\\)','') %>%
    purrr::map(purrr::quietly(as.integer)) %>%
    purrr::map_int('result') %>%
    purrr::map( ~if (is.na(.x)){'+'} else{.x})
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

#' Title
#'
#' Description
#'
#' Details
#'
#' @param x Description for x
#' @param a Description of a
#' @param b
#'   Description of b
#' @section Important:
#' Don't run with scissors!
#' @export
example_func_for_parsing <- function(x,a,b){
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
