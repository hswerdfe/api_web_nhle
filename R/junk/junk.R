

library(httr)
library(jsonlite)
library(tibble)
library(glue)
library(purrr)
library(stringr)
library(tidyverse)
library(digest)

library(stringdist)
library(psych)

library(stringi)






nhl_function_vectorizer <- function(func, arg_name, all_possible_val = 'all', all_possible = NULL){
  function(...) {
    args <- list(...)

    arg_values <- 
      if ( is.null( args[[arg_name]] ) ){
          x<- args |> first()
          args[[1]] <<- NULL
          x
      }else{
        x <- args[[arg_name]]
        args[[arg_name]] <<- NULL
        x
      }
    
    
    if (length(arg_values) == 1) {

      if ( arg_values == all_possible_val ){
        all_possible <- 
          if (  is.function( all_possible) ){
            all_possible()
          }
        
        if ( is.vector(all_possible) ){
          args[[arg_name]] <-  all_possible
          c(!!sym(arg_name) := all_possible, args)
        }
        stop(glue('you passed in {arg_name}={arg_values}, but if you do this `all_possible` must be either a function that returns a vector or a vector itself. Specify  this when you call `nhl_function_vectorizer` '))   
      }
      
      
      result <- func(...)
      result[[arg_name]] <- arg_values
      return(result)
    }
    

        
    
    
    arg_values %>%
      map_dfr(~ {
        args[[arg_name]] <- .x
        result <- do.call(func, args)
        result[[arg_name]] <- .x
        result
      })
  }
}

nhl_teams_v <- nhl_function_vectorizer(nhl_teams,'lang', all_possible = c('en', 'fr', 'ru'))
nhl_teams_v(lang = c('en', 'fr')) 
nhl_teams_v('all')








#' nhl_config
#' 
#'  Retrieve configuration information.
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
nhl_config <- function(lang = 'en'){
  lang <- nhl_param_norm_lang(lang)
  glue::glue('{lang}/config') |>
    nhl_make_url_stats() |>
    nhl_get_url_with_retry()
}




#' nhl_game_meta
#' 
#' Retrieve metadata for game.
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_game_meta(lang='en')
#' 
nhl_game_meta <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  glue::glue('{lang}/game/meta') |>
    url_make_stats() |> 
    get_url_with_retry() |> 
    nhl_pluck()  
}




#' nhl_component_seasons
#' 
#' 
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
nhl_component_seasons <- function(lang = 'en'){
  lang <- nhl_param_norm_lang(lang)
  
  nhl_get_stats('{lang}/componentSeason') 
}


# Not usefull above
#######################












#' nhl_team_report
#'
#' @param lang 
#' @param report 
#'
#' @return
#' @export
#'
#' @examples
nhl_team_report <- function(lang = 'en', report = 'summary'){
  #TODO: fix this thingy
  warning('nhl_team_report IS NOT DONE !!!!')
  lang <- nhl_param_norm_lang(lang)
  
  glue::glue('{lang}/team/{report}') |>
    nhl_make_url_stats() |>
    nhl_get_url_with_retry() |> 
    nhl_pluck()
}






#' nhl_get
#'
#' @param endpoint 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_get('player/8478402/game-log/20152016/2')
nhl_get <- function (endpoint, ...){
  endpoint |> 
    nhl_make_url() |> 
    nhl_get_url_with_retry()
}





#' nhl_param_norm_player
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#'  
#'     nhl_param_norm_player(x = 'Ovechkin')
#'     x = 'Ovec'
#'     nhl_param_norm_player(x = 'gretzky')
#'     nhl_param_norm_player(x = 'Wayne')
#'     nhl_param_norm_player(x = 'Brent Gretzky')
#'     nhl_param_norm_player(x = 'mcda')
nhl_param_norm_player <- function(x){
  roster <- nhl_full_roster()|>
    mutate(full_name = glue('{firstName_default} {lastName_default}' ), 
           full_name_r = glue('{lastName_default}, {firstName_default}' ), 
    )  
  
  if (is.integer(x)){

    if (x %in% roster$id){
      return  (as.character(x))
    }
  
  } 
  x <- as.character(x)
  if (x %in% as.character(roster$id)){
    return (x)
  }
  
  roster_long <- 
    roster |>
    select(id, full_name, full_name_r, firstName_default, lastName_default) |>
    pivot_longer(cols = c(full_name, full_name_r, firstName_default, lastName_default), values_drop_na = TRUE) |>
    distinct()
  
  possibles <- 
    roster_long |>
    filter(nhl_str_base_clean(value) == nhl_str_base_clean(x)) |>
    pull(id) |>
    unique()
      
  if (length(possibles) == 1){
    return(possibles)
  }
  if (length(possibles) > 1){
    msg <- 
      roster_long |>
      filter(id %in% possibles) |>
      filter(name == 'full_name') |>
      mutate(lbl = glue('{id}=`{value}`')) |>
      pull(lbl) |>
      paste0(collapse = '; ') |>
      str_trunc(500)
    stop(glue('player `{x}` is not unique {length(possibles)} options, including... {msg}. Be more specific! 🎯'))
  }
  
  # at this point there are zero matches
  possibles <- 
    roster_long |>
    filter(
      str_detect(
        nhl_str_base_clean(value) , 
        nhl_str_base_clean(x)
      )
    ) |>
    pull(id) |>
    unique()
  
  if (length(possibles) == 1){
    return(possibles)
  }
  if (length(possibles) > 1){
    msg <- 
      roster_long |>
      filter(id %in% possibles) |>
      filter(name == 'full_name') |>
      mutate(lbl = glue('{id}=`{value}`')) |>
      pull(lbl) |>
      paste0(collapse = '; ') |>
      str_trunc(500)
    stop(glue('player `{x}` is not unique {length(possibles)} options, including... {msg}. Be more specific! 🎯'))
  }
  
  # at this point there are zero matches
  x
  possibles_df <- 
    roster_long |>
    mutate(dist = stringdist::stringdist(a = nhl_str_base_clean(value), b = nhl_str_base_clean(x), method = 'jw' )) |>
    arrange(dist) |>
    summarise(score = harmonic.mean(dist), .by = id ) |>
    arrange(score)
    
  msg <- 
    possibles_df |>
    left_join(roster_long, by = join_by(id)) |>
    filter(name == 'full_name') |>
    mutate(lbl = glue('{id}=`{value}`')) |>
    pull(lbl) |>
    paste0(collapse = '; ') |>
    str_trunc(500)
  
  stop(glue('player `{x}` found no matches some things you might mean are ... {msg}. Be more specific! 🎯'))
  
  
  

  return(x)
}




#' nhl_player_games
#'
#' Get Game Log
#'
#' @param player 
#' @param season 
#' @param game_type 
#'
#' @return
#' @export
#'
#' @examples
#'     nhl_player_games(player = 8478402, season = 2015, game_type = 'regular')
#'     nhl_player_games(player = 8478402, season = 2023, game_type = 'playoff')
#'     nhl_player_games('mcdavid', season = 2015, game_type = 'regular')
#'     nhl_player_games('ovech', season = 2022, game_type = 'regular')
nhl_player_games <- function(player, season, game_type){
  season <- nhl_param_norm_season(season)
  game_type <- nhl_param_norm_game_type(game_type)
  player <- nhl_param_norm_player(player)
  
  #########################
  # TODO: this also works but for current time, integrate this later
  # 'player/{player}/game-log/now'
  # 
  glue::glue('player/{player}/game-log/{season}/{game_type}') |>
  nhl_get() |>
  nhl_as_tibble()
}





#' nhl_player
#'
#' Retrieve information for a specific player.
#'
#' @param player 
#'
#' @return
#' @export
#'
#' @examples
#' dat <- nhl_player(8478402)
#' nhl_player('Gretzky')
#' nhl_player('mcdavid')
#' nhl_player('ovech')
#' nhl_player('richard')
#' nhl_player('maurice richard')
#' nhl_player(8478402)$teamLogo
#' nhl_player(8478402)$headshot
#' nhl_player(8478402)$heroImage
nhl_player <- function(player){
  player <- nhl_param_norm_player(player)
  
  glue::glue('player/{player}/landing') |>
  nhl_get() |>
  nhl_as_tibble()
}



#' nhl_seasons
#'
#'Retrieve a vector of all season IDs past & present in the NHL.
#'
#'
#' @return
#' @export
#'
#' @examples
#' 
#'   nhl_seasons()
nhl_seasons <- function(){
  nhl_get('season')
}


#' nhl_param_norm_date
#' 
#'  normalize the date parameter
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
nhl_param_norm_date <- function(x){
  as.character(x)
}



#' nhl_schedule
#'
#'  todo :
#'  Retrieve the schedule calendar for a specific date (i DON'T THINK THIS DOES THIS)
#'
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_schedule()
#'    nhl_schedule(dt = '2024-02-15')
#'    nhl_schedule(dt = '2015-11-12')
#' 
#' 
nhl_schedule <- function(dt = Sys.Date()){
  dt = nhl_param_norm_date(dt)

  glue::glue('schedule-calendar/{dt}') |> 
  nhl_get() |>
  nhl_as_tibble()
}



#' nhl_game_play_by_play
#' 
#'  nhl_game_play_by_play(game = 2021020001) |> pluck('plays') |> View()
#' 
#'
#' @param game 
#'
#' @return
#' @export
#'
#' @examples
nhl_game_play_by_play <- function(game){
  game = nhl_param_norm_game(game)
  
  glue::glue('gamecenter/{game}/play-by-play') |> 
  nhl_get() |>
  nhl_as_tibble()

}


#' nhl_team_roster
#'
#'  get the roster
#'
#' @param team 
#' @param season 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_team_roster('maple leaf', season = 'current')
#'   nhl_team_roster(team = 'ATL', season = 'current')
#'   nhl_team_roster('Canadiens')
#'   nhl_team_roster(team = 'Canadiens', season = 1995)
#'   df <- nhl_team_roster('Nordiques', season = 'all') 
#'   df <- nhl_team_roster(team = 'all', season = 'current')
#'   df <- nhl_team_roster(team = 'all', season = '2001')  
#'   nhl_full_roster <- nhl_team_roster(team = 'all', season = 'all')  
nhl_team_roster <- function(team, season = 'current'){
  
  if (team == 'all'){
    return (
      nhl_teams_vect()|>
        map_dfr(~{
          #.x = 'TOR'
          #season = '19261927'
          nhl_team_roster(team = .x,
                          season = season
                          ) |>
            mutate(team2 = .x)
        })
    )
  }
  
  
  
  team <- nhl_param_norm_team(team)
  
  
  

  if (season == "all"){
    
    dat_roster <-
      nhl_seasons(team) |> 
      map_dfr(~{
        nhl_team_roster(team = team, season = .x) |>
          mutate(season = .x)
      })
    
    return (dat_roster)
  }
  
  
  ##################
  # normalize the season if only one
  season <-
    if (season != "current"){
      season <- nhl_param_norm_season(season)
      if (! season %in% nhl_seasons(team)){
        season_range <- nhl_seasons(team) |> range() |> paste(collapse = '->')
        message(glue('Warning: team = `{team}`, did not seam to play in season = `{season}`, returning blank tibble. try like a season in the range {season_range}, 🤷 maybe.'))
        return (tibble())
      }
      
      season
    }else{
      season
    }
  
  nhl_get_safely <- safely(nhl_get, otherwise = NULL)
  ##################
  # geta as seasons roster
  curr_roster <-
    glue::glue('roster/{team}/{season}') |> 
    nhl_get_safely() |>
    pluck('result') |>
    nhl_as_tibble()
  
  
  ##################
  # add the position type to the dataframes and return as one dataframe
  curr_roster_all <- 
    map2_dfr(curr_roster, names(curr_roster), 
         \(.xx, .pos){
              .xx  |>
              mutate(position_type = .pos)
    })
  return (curr_roster_all)
}


#' nhl_full_roster, 
#'
#' call nhl_full_roster() to get the full roster for all time in all teams
#' 
#' Warning this takes many many requests to the server!!!! and will take a long time, the first time after that you are good to go 🚗. 
#'
#' @return
#' @export
#'
#' @examples
#'    nhl_full_roster()
#' 
nhl_full_roster_TO_CACHE <- function(){
  nhl_team_roster(team = 'all', season = 'all')        
}
nhl_full_roster <- nhl_make_cached_function(nhl_full_roster_TO_CACHE)













#' nhl_game_landing
#'
#' @param game 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_game_landing(2023020204)
#' 
#' 
nhl_game_landing <- function(game){
  game = nhl_param_norm_game(game)
  
  glue::glue('gamecenter/{game}/landing') |> 
    nhl_get() |>
    nhl_as_tibble()
  
}


full_roster() |> filter (id == 8477949) |> View()
# team = 'Nordiques'
# team = nhl_param_norm_team(team)
# dat_roster <-
#   nhl_seasons(team) |> 
#     set_names() |>
#     map(~{
#       nhl_team_roster(team = team, season = .x)
#     })
# 
# cols_in_common <- 
#   Reduce(intersect,
#     map(unname(dat_roster),
#          ~{
#            map(unname(.x),
#                 \(.xx){
#                   colnames(.xx)
#                 }) |> unlist()
#          })
# )
# 
# 
# df_roster <- 
#   map2_dfr(dat_roster, names(dat_roster),
#       \(.x, .season){
#           map2_dfr(.x, names(.x), 
#             \(.xx, .pos){
#               .xx |> select(any_of(cols_in_common)) |>
#                 mutate(position_type = .pos)
#           }) |>
#             mutate(.season = .season)
#       })
# 
# 
# 
#     .x$forwards |>
#       mutate(possition_type = 'forwards', 
#              season = .season
#       )
#     
#   })
# 
# df |> colnames() |> map_dfc(~{
#   if ( inherits(df[[.x]], 'data.frame') ){
#     if (ncol(df[[.x]]) == 1){
#       tibble(!!sym(.x) := df[[.x]][[1]])
#     }else if (ncol(df[[.x]]) > 1){
#       df[[.x]] |> rename_all(\(.nm){paste(.x, .nm, sep = '_')})
#     }
#   }else{
#     df[.x]
#   }
# })
#   df[['firstName']]  |> rename_all(~{paste0('dsfasdf', .x)})|> unnest('firstName', names_sep  = '_')
# df <- a[['19791980']]$forwards 
# colnames(df) <- gsub("\\$default", "", colnames(df))
# df$firstName





#' nhl_game_boxscore
#'
#' @param game 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_game_boxscore(2023020204)
#' 
nhl_game_boxscore <- function(game){
  game = nhl_param_norm_game(game)
  
  glue::glue('gamecenter/{game}/boxscore') |> 
    nhl_get() |>
    nhl_as_tibble()
  
}




#' nhl_gameStory
#'
#' @param game 
#'
#' @return
#' @export
#'
#' @examples
nhl_gameStory <- function(game){
  game = nhl_param_norm_game(game)
  
  glue::glue('wsc/game-story/{game}') |> 
    nhl_get() |>
    nhl_as_tibble()
}



  
 

#' nhl_club_schedule_season
#'
#' @param team 
#' @param season 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_club_schedule_season('TOR',  2023)
#' nhl_club_schedule_season('maple leafs',  '2015-16')
#' nhl_club_schedule_season(team = 'hartford',  season = '1990')
#' nhl_club_schedule_season(team = 'hartford',  season = '1989')
#' nhl_club_schedule_season('hartford',  season = '2024')
#' nhl_club_schedule_season('hartford',  season = 'all')
#' nhl_club_schedule_season('all',  season = 'all')
nhl_club_schedule_season <- function(team, season){


  if (team == 'all'){
    return (
      nhl_teams_vect()|>
        map_dfr(~{
          #.x = 'TOR'
          #season = '19261927'
          nhl_club_schedule_season(
                          team = .x,
                          season = season
          ) |>
            mutate(team2 = .x)
        })
    )
  }
  
  
  
  if (season == "all"){
    
    dat_schedule <-
      nhl_seasons(team) |> 
      map_dfr(~{
        nhl_club_schedule_season(team = team, season = .x) |>
          mutate(season = .x)
      })
    
    return (dat_schedule)
  }

  season = nhl_param_norm_season(season)
  team = nhl_param_norm_team(team) 
  
  
  tmp_sch <- 
    glue::glue('club-schedule-season/{team}/{season}') |>
    nhl_get() |>
    nhl_as_tibble()
  
  
  tmp_sch$games |>
    mutate(
        currentSeason = tmp_sch$tmp_sch, 
        clubTimezone = tmp_sch$clubTimezone, 
        clubUTCOffset = tmp_sch$clubUTCOffset
      )
}



#' nhl_full_schedule
#' 
#'  like nhl_full_roster() it will take a long time the first time this is called.
#'
#' @return
#' @export
#'
#' @examples
#'     nhl_full_schedule()
#' 
nhl_full_schedule_TO_CACHE <- function(){
  nhl_club_schedule_season(team = 'all', season = 'all')        
}
nhl_full_schedule <- nhl_make_cached_function(nhl_full_schedule_TO_CACHE)




#' nhl_schedule
#' 
#'  get the schedule for the whole leage for that date and around that date
#'
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_schedule(dt = '2024-12-25')
#' 
nhl_schedule <- function(dt){
  dt = nhl_param_norm_date(dt)

  
  glue::glue('schedule/{dt}') |>
    nhl_get() |>
    nhl_as_tibble()
}



#' nhl_score
#'
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_score('2024-02-15')
#' 
nhl_score <- function(dt){
  dt = nhl_param_norm_date(dt)
  
  
  glue::glue('score/{dt}') |>
    nhl_get() |>
    nhl_as_tibble()
}






'player/8478402/landing' |> 
  nhl_get()

'player/8478402/game-log/now' |> 
  nhl_get() |>
  nhl_as_tibble()
  


'skater-stats-leaders/current?categories=goals&limit=50' |> 
  nhl_get() |>
  nhl_as_tibble()


'skater-stats-leaders/20222023/2?categories=assists&limit=3' |> 
  nhl_get()



'goalie-stats-leaders/current?categories=wins&limit=5' |>
  nhl_get()



'goalie-stats-leaders/20232024/2?categories=wins&limit=3' |>
  nhl_get()

'where-to-watch'  |> 
  nhl_get()

'player-spotlight' |> 
  nhl_get()


'standings/now'|> 
  nhl_get()



'standings/2023-11-10' |> nhl_get()



'standings-season'  |> nhl_get()




'club-stats/TOR/now' |>  nhl_get()|>
  nhl_as_tibble()



'club-stats-season/TOR' |>  nhl_get() |>
  nhl_as_tibble()




'club-stats/TOR/20232024/2' |> nhl_get() |> nhl_as_tibble()



'scoreboard/TOR/now' |> nhl_get() |> nhl_as_tibble()

'roster/TOR/current' |> nhl_get()

'roster/TOR/20232024' |> nhl_get()


'roster-season/TOR' |> nhl_get()


'prospects/TOR' |> nhl_get()

#' nhl_prospects
#'
#' @param team 
#'
#' @return
#' @export
#'
#' @examples
#'     nhl_prospects(team = 'TOR')
#'     nhl_prospects(team = 'krak')
#'     nhl_prospects(team = 'all')['team']
nhl_prospects <- function(team){
  
  if (team == 'all'){
    return (
      nhl_teams_vect() |>
        map_dfr(~{
            nhl_prospects(.x) |>
            mutate(team = .x)
          }) 
    )
  }
  
  team = nhl_param_norm_team(team)
  
  glue('prospects/{team}') |> 
    nhl_get() |> 
    nhl_as_tibble() %>%
    map2_dfr(., names(.), ~{
      .x |>
      mutate(position_type = .y)
    })
}






nhl_prospects('tor')

