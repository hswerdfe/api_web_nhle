

source(file.path('R', 'source_here.R'))

here_source('cache_vec.R')
here_source('season_team_vector.R')

require(glue)
require(purrr)
require(dplyr)






#' schedule
#' 
#'   get the schedule for any team and season
#'
#' @param team A or a vector of tricode teams, optionally you can use 'all' to get all teams
#' @param season A or a vector of 8  digit seasons, optionally you can use 'all' to get all seasons
#' @param pattern 
#' @param url_type 
#' @param var_nm 
#'
#' @return  returns  the schedule for any team and season
#' @export
#'
#' @examples
#'         schedule(team = c('tor', 'ott'), season = c(20222023, 20232024) )
#'         schedule(team = 'all', season = 20232024 )
#'         schedule(team = 'atl', season = 'all' )  |> select(matches('__'))
#'         cache_vec_save(func = schedule)
#'         
#'         
schedule_BASE <- function(
    team, 
    season,
    pattern = 'club-schedule-season/{team}/{season}',
    url_type = 'base',   
    var_nm = 'games'
){
  #team = 'tor'
  #season = 20232024
  x <- get_url_base(pattern = pattern, url_type = url_type)  
  
  if (is.null(x)){
    return(tibble())
  }
  df <- 
    x$games |>
    mutate(currentSeason = x$currentSeason , 
           clubTimezone = x$clubTimezone , 
           clubUTCOffset = x$clubUTCOffset 
    )
  
  df |>
    colnames() |>
    purrr::map(~{
      #.x = 'tvBroadcasts'
      .v <- df[.x]
      if (is.data.frame(.v[[.x]])){
        .v |> unnest(.x) |> rename_with(\(.nm){paste(.x, .nm, sep = '_')})
      }else if (is.list(.v[[.x]])){
        #.v |> unnest(.x)
        NULL
      }else{
        .v
      }
    }) |>
    purrr::list_cbind()
  
  
}
schedule <- cache_vect_maker(
  func = schedule_BASE,
  x_vec_nms  =  c('team', 'season'),
  all_possible  = list(
    team = teams(),
    season = seasons()
  ), 
  is_valid_combo = is_valid_season
)

