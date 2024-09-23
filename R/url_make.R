

require(glue)
require(purrr)
require(stringr)
require(magrittr)



#' url_make_base, url_make_type, url_make, url_make_stats,  url_make_records
#'
#'    these all make urls that can access the API, try using url_make_type   as like a default
#'
#' @param endpoint 
#' @param base_url 
#' @param protocall 
#' @param params  List of params
#' @param ... 
#'
#'
#' @return a string that is a url
#' @export
#'
#'
#' @examples
#' url_make_base('player/8478402/game-log/20152016/2',  'api-web.nhle.com/v1')
#' url_make_base('some_words',  'api.nhle.com/stats/rest')
#' 
#' 
#' url_make_type('XXXXXX', 'records')
#' url_make_type('XXXXXX', 'stats')
#' url_make_type('XXXXXX', 'base')
#' 
#' url_make_type('player/8478402/game-log/20152016/2', 'base')
#' url_make_type(endpoint  = 'XXXXXX', type = 'BAD_VALUE')
#' url_make_type(endpoint = 'officials', type = 'records',  params = list(active = TRUE))
#' 
#' 
#' 
url_make_base <- function(endpoint,
                          base_url,
                          protocall = 'https', 
                          params =  list(),
                          ...
){
  if (length(params) == 0){
    glue::glue('{protocall}://{base_url}/{endpoint}')  
  } else {
    paste0(
      glue::glue('{protocall}://{base_url}/{endpoint}?cayenneExp=') ,
      purrr::map2(
        names(params), 
        params, 
        ~{glue::glue('{.x} = {.y}')}
      ) |> 
        paste0(collapse = '$') 
      
    ) |> 
      URLencode()
  }
}




url_make_type <-  function(
    endpoint,
    type, 
    ...
){
  base_url = 
    switch(
      stringr::str_to_lower(type),  
      'records' =  'records.nhl.com/site/api',
      'stats' =  'api.nhle.com/stats/rest',
      'base' = 'api-web.nhle.com/v1'
    )
  
  if (is.null(base_url)){
    glue_stop('in `url_make_type`, type=`{type}`, but this is not a valid value!')
  }
  
  url_make_base(endpoint = endpoint,  ..., base_url = base_url)
}



#' url_make
#'
#' @param endpoint 
#' @param protocall 
#' @param base_url 
#' @param version 
#'
#' @return
#' @export
#'
#' @examples
#'   url_make('player/8478402/game-log/20152016/2')
#'   url_make(endpoint  = 'random_words_123456')
#'  url_make(endpoint  = 'random_words_123456', protocall = 'ftp')
#' 
url_make <- function(endpoint, 
                         ...){
  url_make_type(endpoint  = endpoint, type = 'base', ...)
}


#' url_make_stats
#'
#' @param endpoint 
#' @param protocall 
#' @param base_url 
#'
#' @return
#' @export
#'
#' @examples
url_make_stats <- function(endpoint, 
                           ...){
  url_make_type(endpoint  = endpoint, type = 'stats', ...)
}


#' url_make_records
#'
#' @param endpoint 
#' @param protocall 
#' @param base_url 
#'
#' @return
#' @export
#'
#' @examples
#'   url_make_records(endpoint = 'officials')
#'   url_make_records(endpoint = 'officials',  params = list(active = TRUE))
#'   url_make_records(endpoint = 'officials',  params = list(active = FALSE))
#'   url_make_records('award-details')
#'   url_make_records('attendance')
#'   url_make_records('draft-master')
#'   url_make_records('venue')
url_make_records <- function(endpoint,
                             ...){
  url_make_type(endpoint  = endpoint, type = 'records', ...)
}



  


#' url_unmake
#' 
#'   breaks down a url into 
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
#' url_unmake(url = 'https://records.nhl.com/site/api/award-details?cayenneExp=trophyCategoryId=2%20and%20trophyId=16&include=seasonId&include=player.firstName&include=player.lastName&include=player.position&include=player.id&include=value&include=team.id&include=team.franchiseId&include=team.fullName&include=team.placeName&include=team.commonName&include=team.triCode&include=team.logos&include=team.active&include=team.league.abbreviation&include=status&include=imageUrl&include=isRookie&include=summary&include=playerImageUrl&sort=seasonId&dir=DESC')
#' url_unmake(url = 'https://api.nhle.com/stats/rest/en/skater/summary?limit=72&start=17&sort=points&cayenneExp=seasonId=20232024')
url_unmake <- function(url, param_str = 'cayenneExp'){
  
  domain <-  stringr::str_extract(url, c('nhl.com', 'nhle.com')) |>
    keep(.p  = ~ !is.na(.x))
  
  params <- url |> 
    stringr::str_split_1(param_str) |>
    magrittr::extract2(2)  |>
    stringr::str_split_1('&') |>
    stringr::str_remove('^=')  |>
    stringr::str_remove('=$')   |>
    stringr::str_split('=')
  
  
  x <- 
    url |> 
    stringr::str_split_1(param_str) |>
    magrittr::extract2(1) |>
    stringr::str_split_1('://') 
  
  
  protocall <- x[[1]]
  y <- 
    x[[2]] |>
    stringr::str_split_1(domain) |>
    stringr::str_remove('^[/\\.\\?]') |>
    stringr::str_remove('[/\\.\\?]$')
  
  sub_domain <- y[[1]]
  dir_structure <- y[[2]]
      
  param_keys <- 
    params |> 
    purrr::map(~{.x[[1]]}) |> unlist() |> unique()
  
  full_params <- 
    param_keys |>
      purrr::set_names()  |>
      purrr::map(\(.key){
        #.key <- param_keys[[1]]
        #.key = 'include'
        # params[2:length(params)]
        
        params |> 
          keep(.p  = \(.x){.x[[1]] == .key}) |> 
          purrr::map(~{.x[2:length(.x)]}) |> 
          unlist()
      })
  list(
    url  = url, 
    domain = domain, 
    param_str =  param_str,
    protocall = protocall,
    sub_domain = sub_domain,
    dir_structure = dir_structure,
    full_params = full_params  
  )
}



