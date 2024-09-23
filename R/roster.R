source(file.path('R', 'source_here.R'))

here_source('cache_vec.R')
here_source('season_team_vector.R')


require(purrr)
require(dplyr)








  
#' roster
#'
#'   returns a teams roster for a given season
#'
#' @param team A or a vector of tricode teams, optionally you can use 'all' to get all teams
#' @param season A or a vector of 8  digit seasons, optionally you can use 'all' to get all seasons
#' @param pattern   
#' @param url_type 
#' @param var_nm 
#'
#' @return data frame of the roster
#' @export
#'
#' @examples
#'     roster(team = 'ATL', season = 19621963) 
#'     roster(team = 'TOR', season = 19621963)
#'     roster(team = c('TOR',  'DET', 'ATL', 'OTT', 'MTL'), season = c(19631964, 19621963, 19641965))
#'     roster(team = 'all',  season = seasons_team('ATL'))
roster_BASE <- function(
                team, 
                season,  
                pattern = 'roster/{team}/{season}', 
                url_type = 'base',   
                var_nm = 'position_type'
){
  ret_val <- get_url_base(pattern = pattern, url_type = url_type)  
  
  if (is.null(ret_val)){
    return(tibble())
  }
  ret_val |>  nhl_list_bind(var_nm =var_nm)
}
roster <- cache_vect_maker(
  func = roster_BASE,
  x_vec_nms  =  c('team', 'season'),
  all_possible  = list(
    team = teams(),
    season = seasons()
  ), 
  is_valid_combo = is_valid_season
)

# cache_vec_save(roster)
# get(x = 'cache_df', envir = environment(func))
# 
# assign(x = 'cache_df', value = new_cache_df, envir = environment(roster))

