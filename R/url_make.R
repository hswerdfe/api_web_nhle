




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
#'   url_make('random_words_123456')
#' 
#' 
url_make <- function(endpoint, 
                         protocall = 'https', 
                         base_url = 'api-web.nhle.com', 
                         version = 'v1'){
  glue::glue('{protocall}://{base_url}/{version}/{endpoint}')
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
                           protocall = 'https', 
                           base_url = 'api.nhle.com/stats/rest'){
  glue::glue('{protocall}://{base_url}/{endpoint}')
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
                             protocall = 'https', 
                             base_url = 'records.nhl.com/site/api',
                             params =  list()
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



#' url_unmake
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
#' url_unmake(url = 'https://records.nhl.com/site/api/award-details?cayenneExp=trophyCategoryId=2%20and%20trophyId=16&include=seasonId&include=player.firstName&include=player.lastName&include=player.position&include=player.id&include=value&include=team.id&include=team.franchiseId&include=team.fullName&include=team.placeName&include=team.commonName&include=team.triCode&include=team.logos&include=team.active&include=team.league.abbreviation&include=status&include=imageUrl&include=isRookie&include=summary&include=playerImageUrl&sort=seasonId&dir=DESC')
#' url_unmake('https://api.nhle.com/stats/rest/en/skater/summary?limit=72&start=17&sort=points&cayenneExp=seasonId=20232024')
url_unmake <- function(url){
  params <- url |> 
    stringr::str_split_1('cayenneExp') |>
    magrittr::extract2(2)  |>
    stringr::str_split_1('&') |>
    stringr::str_remove('^=')  |>
    stringr::str_remove('=$')   |>
    stringr::str_split('=')
  
  
  x <- 
    url |> 
    stringr::str_split_1('cayenneExp') |>
    magrittr::extract2(1) |>
    stringr::str_split_1('://') 
  
  
  protocall <- x[[1]]
  y <- 
    x[[2]] |>
    stringr::str_split_1('nhl.com') |>
    stringr::str_remove('^[/\\.\\?]') |>
    stringr::str_remove('[/\\.\\?]$')
  
  sub_domain <- y[[1]]
  dir_structure <- y[[2]]
      
  param_keys <- 
    params |> 
    purrr::map(~{.x[[1]]}) |> unlist() |> unique()
  
  full_params <- 
    param_keys |> 
      set_names()  |>
      purrr::map(\(.key){
        #.key <- param_keys[[1]]
        #.key = 'include'
        params[2:length(params)]
        params |> 
          keep(.p  = \(.x){.x[[1]] == .key}) |> 
          purrr::map(~{.x[2:length(.x)]}) |> 
          unlist()
      })
  list(
    protocall = protocall,
    sub_domain = sub_domain,
    dir_structure = dir_structure,
    full_params = full_params  
  )
}



