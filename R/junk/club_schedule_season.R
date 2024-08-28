


#' nhl_club_schedule_season
#'
#' @param team 
#' @param season 
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
nhl_club_schedule_season_TO_VETORIZE <- function(team, season, lang){
  season <- nhl_param_norm_season(season)
  team <- nhl_param_norm_team(team, lang = lang)

  x <- nhl_get('club-schedule-season/{team}/{season}') 
  x$games |>
    mutate(currentSeason = x$currentSeason , 
           clubTimezone = x$clubTimezone , 
           clubUTCOffset = x$clubUTCOffset 
           )
}

nhl_club_schedule_season <- function_vectorizer(
  func = nhl_club_schedule_season_TO_VETORIZE,
  arg_name = 'season',
  all_possible = nhl_roster_season
)


#' nhl_season_full
#' 
#'    returns the schedule for every team in every season!
#'
#' @param team   vectorized team a single team or 'all'
#' @param season  vectorized season, a single season, or 'all'
#' @param lang   a single language to work with
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_season_full(team = 'tor', season = 1962, lang ='en')
#'    nhl_season_full('maple leafs', '1977', 'en')
#'    nhl_season_full(team = 'all', season = 'all', lang = 'en')
#'    nhl_season_full(team = 'tor', season = 'all', lang = 'en')
#'    nhl_season_full(team = 'all', season = '1990', lang = 'fr')
#'    nhl_season_full(team = 'DET', season = '2024', lang = 'ru')
#'    
nhl_club_schedule_season_full_TO_CACHE <- function_vectorizer(
  func = nhl_club_schedule_season,
  arg_name = 'team',
  all_possible = nhl_prama_norm_teams_all
)
nhl_season_full <- make_cached_function(\(team, season, lang){
  nhl_club_schedule_season_full_TO_CACHE(
    team = team,
    season = season,
    lang = lang
  )
})
# 
# 
# 
# here_source('nhl_param_norm.R')
# here_source('nhl_stat.R')
# here_source('nhl.R')
# nhl_season_full(team = 'tor', season = '2023', lang = 'en')
# nhl_season_full(team = 'tor', season = c(1952, '2023'), lang = 'en')
# x <- nhl_season_full(team = 'tor', season = 'all', lang = 'en')
# x$id
# View(x)
# nhl_season_full(team = 'all', season = 'all', lang = 'en')
# 
# 
# 
# currentSeason
# clubTimezone
# clubUTCOffset