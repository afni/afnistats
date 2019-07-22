main_func <- function(){
  user_args = commandArgs(trailingOnly = TRUE)
  parser <- create_parser_from_function("MBA","afnistats")
  # mba_ops <- parser$parse_args(user_args)
  MBA(mba_ops)
}

create_parser_from_function <- function(topic,package){
  system.file("extdata", "example_parsed_args_1.rds", package = "afnistats")
  example_parsed_args_1
}

#' parse_help_topic
#'
#' @param topic Function for which to get help
#' @param package_name Package from which the function comes
#'
#' @return Parsed help
parse_help_topic <- function(topic,package_name){
  help_rd <- get_help_topic(topic,package_name)
  help_val <- Rd2list(help_rd)
  help_val
}



#' get_help_topic
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


#' Title
#'
#' Description
#'
#' Details
#'
#' @param x,y,z Descriptions for x, y, z
#' @param a Description of a
#' @param b
#'   Description of b
#' @section Important:
#' Don't run with scissors!
#' @export
example_func_for_parsing <- function(x,y,z){
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

