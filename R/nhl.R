
source(file.path('R', 'source_here.R'))
here_source('nhl_param_norm.R')
here_source('request_wrapper.R')
here_source('vectorizer.R')
here_source('nhl_stat.R')

require(glue)
require(purrr)
require(tibble)









# team = 'tor'
# team = nhl_param_norm_team(team)
# season = nhl_param_norm_season('2023')








#' nhl_roster_TO_VETORIZE
#'
#' @param team 
#' @param season 
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_roster_TO_VETORIZE(team = 'tor', season = 1962, 'en')
#'    nhl_roster_TO_VETORIZE('maple leafs', '1977', 'en')
#'    nhl_roster_TO_VETORIZE(team = 'ATL', season = 1962, 'en')
nhl_roster_TO_VETORIZE <- function(team, season, lang){
  team <- nhl_param_norm_team(team, lang)
  season <- nhl_param_norm_season(season)
  
  
  if (! season %in% nhl_roster_season(team, lang)){
    return(tibble::tibble())
  }
  nhl_get('roster/{team}/{season}')  |>
  nhl_list_bind(var_nm = 'position_type')
}



#' nhl_seasons
#'   vector of all NHL seasons
#' @return
#' @export
#'
#' @examples
#'     nhl_seasons_league()
nhl_seasons_league <- function(){
  nhl_get('season') |> unlist()
}


#' nhl_roster_season, nhl_seasons
#'
#': Return a list of all of the seasons that the team played.
#'
#' @param team 
#' @param lang 
#' @param ...  Ignored
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_roster_season(team = 'Nordiques', lang = 'en')
#'   nhl_roster_season(team = 'NHL', lang = 'en')
#'   nhl_roster_season(team = 'TBD', lang = 'en')
#'   nhl_roster_season(team = 'ATL', lang = 'en')
nhl_roster_season <- function(team, lang, ...){
  lang = nhl_param_norm_lang(lang)
  team = nhl_param_norm_team(team, lang)

  ret_val <- nhl_get('roster-season/{team}')  
  if (length(ret_val) == 0){
    return(character(0) |> unlist())
  }
  return (ret_val |> unlist())
}
nhl_seasons <- nhl_roster_season





#' nhl_roster
#' 
#'    returns a teams roster
#'
#' @param team   must be a single team
#' @param season  vectorized season, a single season, or 'all'
#' @param lang   a single language to work with
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_roster(team = 'tor', season = 1962, 'en')
#'    nhl_roster('maple leafs', season = c(1977, 1978), 'en')
#'    nhl_roster('maple leafs', season = 1977, 'en')
#'    nhl_roster(team = 'ATL', season = 1962, 'en')
#'    nhl_roster(team = 'ATL', season = 1962, 'en')
nhl_roster <- function_vectorizer(
  func = nhl_roster_TO_VETORIZE,
  arg_name = 'season',
  all_possible = nhl_roster_season
)
nhl_seasons()


#' nhl_roster_full
#' 
#'    returns the full roster for all the NHL for every year and every teamroster
#'
#' @param team   vectorized team a single team or 'all'
#' @param season  vectorized season, a single season, or 'all'
#' @param lang   a single language to work with
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_roster_full(team = 'tor', season = 1962, lang ='en')
#'    nhl_roster_full('maple leafs', '1977', 'en')
#'    nhl_roster_full(team = 'all', season = 'all', lang = 'en')
#'    nhl_roster_full(team = 'tor', season = 'all', lang = 'en')
#'    nhl_roster_full(team = 'all', season = '1990', lang = 'fr')
#'    nhl_roster_full(team = 'DET', season = '2024', lang = 'ru')
#'    
nhl_roster_full_TO_CACHE <- function_vectorizer(
  func = nhl_roster,
  arg_name = 'team',
  all_possible = nhl_prama_norm_teams_all
)
nhl_roster_full <- make_cached_function(\(team, season, lang){
  nhl_roster_full_TO_CACHE(
    team = team,
    season = season,
    lang = lang
  )
})












#' nhl_get
#'
#' @param pattern 
#' @param .envir 

#'
#' @return
#' @export
#'
#' @examples
#'   nhl_get('player/8478402/game-log/20152016/2')
nhl_get <- function(pattern, .envir =  parent.frame()){
  glue::glue(pattern, .envir = .envir) |>
    url_make() |>
    get_url_with_retry()  |>
    nhl_as_tibble()
}
nhl_get_safely <- purrr::safely(nhl_get, otherwise = NULL)









#' nhl_player
#'
#' Retrieve information for a specific player. this will include recent games, awards and season, and career stats
#'
#' @param player 
#' @param lang language two letter
#'
#' @return
#' @export
#'
#' @examples
#' dat <- nhl_player(8478402)
#' nhl_player('Gretzky', lang = 'en')
#' nhl_player('mcdavid', lang = 'en')
#' nhl_player('ovech', lang = 'en')
#' 
#' nhl_player('richard', lang = 'en')
#' nhl_player(player = 'maurice richard', lang = 'en')
#' nhl_player(8478402, lang = 'en')$teamLogo
#' nhl_player(8478402, lang = 'en')$headshot
#' nhl_player(8478402, lang = 'en')$heroImage
nhl_player <- function(player, lang){
  player <- nhl_param_norm_player(player, lang = lang)
  
  glue::glue('player/{player}/landing') |>
    nhl_get() |>
    nhl_as_tibble()
}


#' nhl_player_to_pluck
#'
#' @param player  playerID or player name
#' @param lang    language to return 
#' @param pluck   part of the landing page to get
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_player_to_pluck(player = 'maurice richard', lang = 'en', 'awards')
#' 
nhl_player_to_pluck <- function(player, lang, pluck){
  ret <- nhl_player(player, lang) 
  
  ret_2 <-   
    ret |> 
    nhl_pluck(pluck) |> 
    dplyr::mutate(playerId = ret$playerId)
  
  if ('seasons' %in% colnames(ret_2)){
    ret_2 |> unnest(seasons)
  }else{
    ret_2
  }
}


#' nhl_all_players_vec
#' 
#'  returns a vector of playerIDs that represent all the players that have ever played in the league
#'
#' @param lang language
#' @param ... ignored
#'
#' @return
#' @export
#'
#' @examples
#'     nhl_all_players_vec(lang = 'en')
#' 
nhl_all_players_vec <- function(lang, ...){
  nhl_roster_full(team = 'all', season = 'all', lang = lang) |> pull(id) |> unique() 
}

##################################
# Warning : This can take a while  !!!
# We are getting every roster for every year, for every team ever.🤷
G_CACHE_ALL_PLAYERS_VEC <- nhl_all_players_vec(lang = 'en')



#' nhl_player_awards_TO_VETORIZE
#'
#' @param player  playerID or player name
#' @param lang    language to return 
#'
#' @return
#' @export
#'
#' @examples
#'      nhl_player_awards_TO_VETORIZE(player = 'Brent Gretzky', lang = 'en')
#'      nhl_player_awards_TO_VETORIZE(player = 'ovech', 'en')
#'      nhl_player_awards(player = c('Brent Gretzky', 'ovech', 'Wayne Gretzky'), lang = 'en')
#' 
nhl_player_awards_TO_VETORIZE <- function(player, lang){
  nhl_player_to_pluck(player =  player, lang = lang , pluck = 'awards')
}


#' nhl_player_awards
#'
#' @param player  playerID or player name can be a vector of names or the word 'all'
#' @param lang    language to return 
#'
#' @return
#' @export
#'
#' @examples
#'      nhl_player_awards(player = c('Brent Gretzky', 'ovech', 'Wayne Gretzky'), lang = 'en')
#'      nhl_player_awards(player = 'all', lang = 'en')
nhl_player_awards_TO_CACHE <- function_vectorizer(
  func = nhl_player_awards_TO_VETORIZE, 
  arg_name = 'player',
  all_possible = G_CACHE_ALL_PLAYERS_VEC
)
nhl_player_awards <- make_cached_function(\(player, lang){
  nhl_player_awards_TO_CACHE(
    player = player,
    lang = lang
  )
})





#' nhl_player_season_totals_TO_VETORIZE
#'
#' @param player  playerID or player name
#' @param lang    language to return 
#'
#' @return
#' @export
#'
#' @examples
#'      nhl_player_season_totals_TO_VETORIZE(player = 'Brent Gretzky', lang = 'en')
#'      nhl_player_season_totals_TO_VETORIZE(player = 'ovech', 'en')
#'      nhl_player_season_totals(player = c('Brent Gretzky', 'ovech', 'Wayne Gretzky'), lang = 'en')
#' 
nhl_player_season_totals_TO_VETORIZE <- function(player, lang){
  nhl_player_to_pluck(player =  player, lang = lang , pluck = 'seasonTotals')
}



#' nhl_player_season_totals
#'
#' @param player  playerID or player name can be a vector of names or the word 'all'
#' @param lang    language to return 
#'
#' @return
#' @export
#'
#' @examples
#'      nhl_player_season_totals(player = c('Brent Gretzky', 'ovech', 'Wayne Gretzky'), lang = 'en')
#'      nhl_player_season_totals(player = 'all', lang = 'en')
#'      ids <- sample(G_CACHE_ALL_PLAYERS_VEC, 25)
#'      nhl_player_season_totals(player = ids, lang = 'en')
nhl_player_season_totals_TO_CACHE <- function_vectorizer(
  func = nhl_player_season_totals_TO_VETORIZE, 
  arg_name = 'player',
  all_possible = G_CACHE_ALL_PLAYERS_VEC
)
nhl_player_season_totals <- make_cached_function(\(player, lang){
  nhl_player_season_totals_TO_CACHE(
    player = player,
    lang = lang
  )
})





# 
# 
# x <- nhl_roster_s(team = 'maple leafs', season = 'all', lang = 'en')
# names(x)
# 
# nhl_get('roster/{team}/{season}')
# 
# 
# 
# 
# 
# 
# nhl_roster()
# nhl_get('roster/{team}/{season}')  |>
#   nhl_list_bind(var_nm = 'position_type')
#   (\(x) purrr::map2(x, names(x), \(.val, .nm) {
#     .val |>
#       dplyr::mutate(position_type = .nm)
#   }))() |>
#   purrr::list_rbind()
# 
# 
# curr_roster <-
#   glue::glue('roster/{team}/{season}') |>
#   nhl_get_safely() |>
#   pluck('result') |>
#   nhl_as_tibble()
# 
# 
# 
# 
# 
# 
# G_CACHE['97198cd4e7270e4cd5d6034ea6385efb']
