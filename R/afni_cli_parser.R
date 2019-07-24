main_func <- function(){
  user_args = commandArgs(trailingOnly = TRUE)
  parser <- create_parser_from_function("MBA","afnistats")
  # mba_ops <- parser$parse_args(user_args)
  MBA(mba_ops)
}

create_parser_from_function <- function(topic="MBA",package="afnistats"){
  parsed_help <- parse_help_topic(topic,package)
  parser <- create_parser_from_help(parsed_help)
  parser
}

create_parser_from_help <- function(parsed_help){

  parser <- argparse::ArgumentParser(description=fix_help_string(parsed_help$description),add_help=FALSE)
  parser$add_argument('-help','-h',action='help')
  df_arg <- get_df_arg(parsed_help)
  df_arg %>%
    dplyr::group_by(1:dplyr::n()) %>%
    tidyr::nest() %>%
    dplyr::mutate(parsed = purrr::map(data, ~ add_arg(.x,parser)))
  parser

  }
add_arg <- function(df,parser){
  # option flags that take no arguments are treated differently
  if (!is.na(df$nargs == 0) && df$nargs == 0){
    df <- df %>%
      dplyr::select(-c(nargs,type,metavar)) %>%
      dplyr::mutate(action = 'store_true')
  }
  arg_list <- df  %>% purrr::transpose() %>% tibble::deframe()
  do.call(parser$add_argument,c(stringr::str_c('-',df$dest),arg_list))
  # if (stringr::str_detect(capture.output(parser$print_help()),'raise ValueError')){stop("parser broke")}
  help_val <- capture.output(parser$print_help())
  if (sum(stringr::str_detect(help_val,'raise ValueError'))) {
    stop(stringr::str_c("parser broken for: ",arg_list$dest))
  }

  TRUE
}

fix_help_string <- function(string){
  stringr::str_replace_all(string,'\\n',' ')
}



get_df_arg <- function(parsed_topic){
  arguments <- purrr::flatten(parsed_topic['arguments'])
  df_arg <- purrr::map(arguments,'description') %>%
    purrr::set_names(purrr::map(arguments,'arg')) %>%
    tibble::enframe() %>%
    tidyr::separate(value,sep=':',c('metavar','type','nargs','help'),extra='merge') %>%
    dplyr::mutate(
      nargs = fix_nargs(nargs),
      help = fix_help_string(help),

      ) %>%
    dplyr::rename(dest = name)

  df_arg
}

fix_nargs <- function(nargs){
  nargs %>%
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
  help_file <- pkgload::dev_help(topic,package_name)

  # below fails because it is not rdx
  # rd <- utils:::.getHelpFile(help_file$path)
  rd <- tools::parse_Rd(help_file$path)
  rd
}
