

require(glue)

#' glue_XX
#' 
#'  functions glue_stop, glue_print, glue_message, glue_warning
#'  
#'  
#'   are simple replacement for 
#'    print(glue('some string with {var}'))
#'
#'  glue_do takes a .func
#'
#' @param .x passed to glue
#' @param ...   passed to glue 
#' @param .envir    passed to glue defaults to parent.frame()
#' @param .func  results of glue passed to this
#'
#' @return reults of .func
#' @export
#'
#' @examples
#'     world <- 'mars'
#'     glue_message('HELLO {world}!')
glue_do <- function(.x, ..., .envir = parent.frame(), .func){
  .func(glue::glue(.x, ..., .envir = .envir))
}
glue_stop <- function(.x, ..., .envir = parent.frame(), .func = stop){
  glue_do(.x, ..., .envir = .envir, .func = .func )
}

glue_print <- function(.x, ..., .envir = parent.frame(), .func = print){
  glue_do(.x, ..., .envir = .envir, .func = .func )
}
glue_message <- function(.x, ..., .envir = parent.frame(), .func = message){
  glue_do(.x, ..., .envir = .envir, .func = .func )
}
glue_warning <- function(.x, ..., .envir = parent.frame(), .func = warning){
  glue_do(.x, ..., .envir = .envir, .func = .func )
}


