

source(file.path('R', 'source_here.R'))

here_source('url_get.R')


require(purrr)
require(dplyr)




#' seasons_nhl, seasons_team, seasons
#' 
#'   vector of all NHL seasons or seasons for a team
#'   
#' @return   vector of all NHL seasons
#' @export
#'
#' @examples
#'     seasons_nhl()
#'        seasons_team('XXX')
#'        seasons_team('UTA')
#'        seasons_team('TOR')
#'        seasons_team('ott')
#'        seasons_team('atl')
#'        seasons('atl')
#'        seasons()
seasons_nhl <- function(){
  get_url_base('season', url_type = 'base') |> unlist()
}


seasons_team <- function(team, ..., pattern = 'roster-season/{team}',   get_func = get_url_possibly_cached){
  ret_val <- get_url_base(pattern = pattern, url_type = 'base', get_func = get_func, ...)  
  if (length(ret_val) == 0){
    return(integer(0) |> unlist())
  }
  return (ret_val |> unlist())
}


seasons  <- function(team = NULL, ...){
  if (is.null(team)){
    seasons_nhl()
  }else{
    seasons_team(team = team , ...)
  }
}

#' teams
#'
#' @param lang defaults to 'en'
#'
#' @return a vector of tricodes
#' @export
#'
#' @examples
#' teams_df() |> filter(rawTricode == 'UTA')
#' teams()
#'   
teams_df <- function(lang  = 'en',  ..., get_func = get_url_possibly_cached){
  get_url_base(pattern = '{lang}/team', url_type = 'stats', ..., get_func = get_func)
}
teams <- function(lang  = 'en',  ...,  get_func = get_url_possibly_cached){
  get_url_base(pattern = '{lang}/team', url_type = 'stats', ...,  get_func  = get_func) |>
    pull(rawTricode) |>
    unique()
}




#' is_valid_roster_season
#'
#' @param team Three letter team name
#' @param season 8 digit season 
#' @param ... 
#'
#' @return a logical value
#' @export
#'
#' @examples
#'       is_valid_season('OTT', 19621963 )
#'        is_valid_season('tor', 19621963 )
#'        is_valid_season('ott', 19621963 ) 
is_valid_season <-function(team, season, ...){
  season %in% seasons(team)
}


