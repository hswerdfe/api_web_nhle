


source(file.path('R', 'source_here.R'))

here_source('request_wrapper.R')
here_source('util.R')
here_source('url_make.R')


require(purrr)
require(stringr)

G_USE_API_DIRECT_CACHE =  FALSE

get_url_possibly_cached <-  purrr::possibly(get_url_with_retry, otherwise = NULL)
get_url_possibly_not_cached <-  purrr::possibly(get_url_with_retry_TO_CACHE, otherwise = NULL)
get_url_possibly <- 
  if (G_USE_API_DIRECT_CACHE){
    get_url_possibly_cached
  }else{
    get_url_possibly_not_cached
  }



#' get_url_base
#'
#' @param pattern 
#' @param url_type 
#' @param ... passed to url_make_type
#' @param .envir, default parent.frame()
#' @param get_func 
#' @param pluck_nm 
#'
#' @return
#' @export
#'
#' @examples
#'     get_url_base(pattern ='{lang}/team',url_type = 'stats')
#'     get_url_base(pattern ='player/8478402/game-log/20152016/2', url_type = 'base')
#'     get_url_base(pattern ='officials', url_type = 'records',  params = list(active = TRUE))
#'     get_url_base(pattern ='officials', url_type = 'records',  params = list(active = FALSE)) 
#'     get_url_base(pattern ='officials', url_type = 'records') 
get_url_base <- function(
    pattern,
    url_type,
    ...,
    .envir =  parent.frame(),
    get_func = get_url_possibly,
    pluck_nm = 'data'
    
){
  ret_val <- glue::glue(pattern, .envir = .envir) |>
    url_make_type(type = url_type, ...) |>
    get_func() 
  
  final_val <- switch(
    stringr::str_to_lower(url_type), 
    'base'  = nhl_as_tibble(ret_val), 
    'stats'  = nhl_pluck(ret_val,  nm = pluck_nm) , 
    'records' = nhl_pluck(ret_val,  nm = pluck_nm)
  )
  
  if (is.null(final_val)){
    glue_stop('In get_url_base url_type=`{url_type}`, this should not be.')
  }
  final_val
}
