


source(file.path('R', 'source_here.R'))
here_source('nhl_param_norm.R')
here_source('request_wrapper.R')
here_source('vectorizer.R')
here_source('url_make.R')


require(glue)




#' nhl_get_records
#'
#' @param pattern 
#' @param .envir 
#' @param pluck_nm 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_get_records('officials',  params = list(active = TRUE))
#'   nhl_get_records('officials',  params = list(active = FALSE))
#'   nhl_get_records('officials')
#'   nhl_get_records('award-details')
#'   nhl_get_records('attendance')
#'   nhl_get_records('draft-master')
#'   nhl_get_records('venue')
nhl_get_records <- function(pattern, .envir =  parent.frame(), pluck_nm = 'data', ...){
  glue::glue(pattern, .envir = .envir ) |>
    url_make_records(...) |> 
    get_url_with_retry() |>
    nhl_pluck(nm = pluck_nm)
  
}


#' nhl_officials
#'
#' @param active 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples 
#'   nhl_officials()
#'   nhl_officials(active = TRUE)
#'   nhl_officials(active = FALSE)
nhl_officials <- function(active = NULL, ...){
  param <- 
    if ( is.null(active) ){
      list()
    } else {
      list(active = active)
    }
  'officials' |> 
    nhl_get_records(param = param, ...)

}

