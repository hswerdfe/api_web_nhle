



library(ISOcodes)
data(ISO_639_2)


source(file.path('R', 'source_here.R'))
here_source('glue_do.R')


require(tibble)
require(dplyr)
require(stringr)

#' nhl_param_norm_lang
#'
#' Normalize a language parameter
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#'     nhl_param_norm_lang('En')
#'     nhl_param_norm_lang('fre')
#'     nhl_param_norm_lang('RuSsIaN')
nhl_param_norm_lang <- function(x){
  langs <- 
    ISO_639_2 |> 
    tibble::tibble() |> 
    dplyr::filter(! is.na(Alpha_2 ))  
  
  
  if (stringr::str_length(x) == 3){
    return (langs |> dplyr::filter(Alpha_3_B == stringr::str_to_lower(x)) |> dplyr::pull(Alpha_2 ) |> dplyr::first())
  }
  
  
  if (stringr::str_length(x) == 2){
    return (stringr::str_to_lower(x))
  }  
  
  return (langs |> dplyr::filter(stringr::str_to_lower(Name) == stringr::str_to_lower(x)) |>  dplyr::pull(Alpha_2 ) |> dplyr::first())
}








#' nhl_param_norm_langs_allowed
#'
#' @param ...  ignored
#'
#' @return
#' @export
#'
#' @examples   
#'    nhl_param_norm_langs_allowed()
nhl_param_norm_langs_allowed <- function(...){
  ISO_639_2 |> 
    tibble::tibble() |> 
    dplyr::filter(! is.na(Alpha_2 )) |> 
    dplyr::pull(Alpha_2) |>
    unique()
}






#' nhl_param_norm_game
#'
#' this will normalize the game_id parameter, to be a game_id number but as a character
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#'  nhl_param_norm_game(x = '2021020001')
nhl_param_norm_game <- function(x){
  #
  # TODO : allow different forms of Game_ID
  #
  as.character(x)
}







#' nhl_param_norm_yr
#'
#' @param yr 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_param_norm_yr(20)
#' nhl_param_norm_yr("21")
#' nhl_param_norm_yr("2021")
#' nhl_param_norm_yr(2022)
#' nhl_param_norm_yr('NOW')
nhl_param_norm_yr <- function(yr){
  if (is.integer(yr)){
    yr = as.character(yr)
  }
  if (stringr::str_to_lower(yr) %in% c('now', 'today')){
    return (format(Sys.Date(), "%Y"))
  }
  
  
  
  if (stringr::str_length(yr) == 2){
    paste0('20', yr)
  }else if (stringr::str_length(yr) == 4){
    yr
  }else{
    glue_stop('📅 Failed to normalize year `{yr}`')
  }
  
}




#' nhl_param_norm_season
#'
#' @param season 
#'
#' @return
#' @export
#'
#' @examples
#'  nhl_param_norm_season("2023")
#'  nhl_param_norm_season(20232024)
#'  nhl_param_norm_season(season = 'Now')
#'  nhl_param_norm_season(season = '2020-21')
#'  nhl_param_norm_season(season = '2020-2021')
nhl_param_norm_season <- function(season){
  
  
  if (is.integer(season) | is.numeric(season)){
    season <- as.character(season)
  }
  
  if (stringr::str_to_lower(season) %in% c('now', 'today')){
    season = format(Sys.Date(), "%Y")
  }
  
  if (stringr::str_detect(season, "^[0-9]+$")){
    if (stringr::str_length(season) == 8) {
      return (season)
    } else if (stringr::str_length(season) == 4){
      return (paste0(season , as.character(as.integer(season) + 1)))
    } else{
      glue_stop('📅 Failed to normalize season `{season}`')
    }
  } else {
    return (
      stringr::str_split_1(season, '-') |> 
        stringr::str_squish() |>
        sapply(nhl_param_norm_yr) |> 
        paste0(collapse = '')
    )
  } 
  glue_stop('📅 Failed to normalize season `{season}`')
}










#' nhl_param_norm_game_type
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_param_norm_game_type('Regular')
#' nhl_param_norm_game_type('2')
#' nhl_param_norm_game_type('some special words')
nhl_param_norm_game_type <- function(x){
  if (is.integer(x)){
    x <- as.character(x)
  }  
  
  if (stringr::str_squish(stringr::str_to_lower(x)) %in% c('2', 'regular', 'regular_season', 'season', 'regular-season') ){
    return ('2')
  }
  if (stringr::str_squish(stringr::str_to_lower(x)) %in% c('playoffs', 'playoff', 'play-off', 'play-offs') ){
    return ('3')
  }  
  glue_stop('🏒 Failed to normalize game type`{x}`')
}







#' nhl_param_norm_team
#' 
#'  Find a team
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' nhl_param_norm_team('tor', 'en')
#' 
#' nhl_param_norm_team('Maple Leaf', 'en')
#' nhl_param_norm_team(x='Maple Leaf', lang = 'ru') 
#' nhl_param_norm_team('toronto')  # FAILS CAUS MULTIPLE TORONTO ENTRIES
#' 
#' 
nhl_param_norm_team <- function(x, lang){
  nhl_search_df(df = nhl_teams(lang), x = x, rot_cols = c('fullName', 'id', 'triCode'), key = 'rawTricode')
}


#' nhl_param_norm_player
#'
#' @param x 
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'  
#'     nhl_param_norm_player(x = 'Ovechkin', lang = 'en')
#'     nhl_param_norm_player(x = 'gretzky', lang = 'en')
#'     nhl_param_norm_player(x = 'Wayne', lang = 'en')
#'     nhl_param_norm_player(x = 'Brent Gretzky', lang = 'en')
#'     nhl_param_norm_player(x = 'mcda', lang = 'en')
nhl_param_norm_player <- function(x, lang){
  df <- 
    nhl_roster_full(team = 'all', season = 'all', lang = lang) |> 
    dplyr::select( 
      id,
      matches ( '^(last|first)Name_.*$' )
    ) |>
    tidyr::pivot_longer(cols = matches ( '^(last|first)Name_.*$' ), values_drop_na = TRUE) |>
    tidyr::separate(name, into = c('name','lang'), sep = 'Name_') |>
    dplyr::distinct() |>
    # dplyr::group_by(id, lang, name) %>%
    # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    # dplyr::filter(n > 1L) 
    tidyr::pivot_wider(names_from = name, values_from = 'value', values_fn  = ~{paste0(.x, collapse = ' ')}) |>
    dplyr::mutate(
              full = paste0(first, ' ', last), 
              full_2 = paste0(last, ', ', first)
           )
  
  nhl_search_df(df = df, x = x, rot_cols = c('full', 'full_2'), key = 'id')
}
