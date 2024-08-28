

source(file.path('R', 'source_here.R'))
here_source('nhl_param_norm.R')
here_source('request_wrapper.R')
here_source('vectorizer.R')
here_source('url_make.R')


#' nhl_get_stats 
#' 
#'  The API is in two sections this gets stuff from the stats portion of the API
#' 
#'
#' @param pattern 
#' @param .envir 
#' @param pluck_nm 
#'
#' @return
#' @export
#'
#' @examples
#'   lang = 'en'
#'   nhl_get_stats('{lang}/team')
#' 
nhl_get_stats <- function(pattern, .envir =  parent.frame(), pluck_nm = 'data'){
  glue::glue(pattern, .envir = .envir ) |>
    url_make_stats() |> 
    get_url_with_retry() |> 
    nhl_pluck(nm = pluck_nm)
}


#' nhl_ping
#'
#' Ping the server to check connectivity. This one will always make the request and not return a cache.
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_ping()
#' 
#' 
nhl_ping <- function(){
  glue::glue('ping') |>
    url_make_stats() |> 
    get_url_with_retry(FORCE_REFRESH_AFTER_SEC = -1) |> 
    nhl_pluck(nm ='success')
}




#' nhl_teams
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_teams(lang = 'fr')
#'   nhl_teams(lang = 'eng')
#'   nhl_teams('de')
#'   nhl_teams('sk')
#'   nhl_teams('russian')
#'   nhl_teams(c('russian', 'fr'))
#'  nhl_teams('all')
nhl_teams_to_vectorise <- function(lang, ...){
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/team')
}






#' nhl_teams_vect
#' 
#'  vector of all 3 char team names
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_prama_norm_teams_all(lang = 'en')
#'   nhl_prama_norm_teams_all('english')
nhl_prama_norm_teams_all <- function(...){
  nhl_teams(...) |> 
    filter(! is.na(franchiseId) ) |>
    dplyr::pull(rawTricode) |> 
    unique()
}




#' nhl_country
#' 
#'  get a dataframe of countries
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_country('en')
#'   nhl_country('fr')
#'   nhl_country('ru')$imageUrl
#'   nhl_country('all')
#'   
#'   nhl_country('xx') # Fails
nhl_country_to_vectorise<- function(lang){
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/country')  
}







#' nhl_shift_charts
#' 
#'  Retrieve shift charts for a specific game.
#'
#' @param game 
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_shift_charts(game = '2021020001', lang = 'en')
#'   nhl_shift_charts(game = '2021020001', lang = 'fr') 
#'  nhl_shift_charts(game = '2021020001', lang = 'all') 
#'  nhl_shift_charts(game = '2021020001', lang = 'RU') 
nhl_shift_charts_TO_VECTORISE <- function(game, lang){
  game <- nhl_param_norm_game(game)
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/shiftcharts?cayenneExp=gameId={game}')
}










#' nhl_glossary
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_glossary('fre')
#'   nhl_glossary('russian')
#'   nhl_glossary('de')
#'   nhl_glossary('all')
#' 
nhl_glossary_TO_VECTORISE <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/glossary')
}






#' nhl_games
#'
#'   Retrieve game information.
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_games('ru')
#'  nhl_games('en')
#'  nhl_games('all')
nhl_games_TO_VECTORISE <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  
  nhl_get_stats('{lang}/game') 
}







#' nhl_seasons
#'
#'   Retrieve season information as a tibble
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'   nhl_seasons('en')
#' 
nhl_seasons_TO_VECTORISE <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/season')
}






#' nhl_franchises
#'
#'    Retrieve list of all franchises.
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'  nhl_franchises('en')
#'  nhl_franchises('ru')
#'  nhl_franchises('all')
nhl_franchises_TO_VECTORISE <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  nhl_get_stats('{lang}/franchise') 
}




#' nhl_draft
#' 
#'  Retrieve some very basic draft information.
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'  nhl_draft('en')
#' 
nhl_draft_TO_VECTORISE <- function(lang){
  lang <- nhl_param_norm_lang(lang)
  
  nhl_get_stats('{lang}/draft') 
}






# Define a list of function names and their corresponding functions
functions_to_vectorize <- list(
  nhl_teams = nhl_teams_to_vectorise,
  nhl_country = nhl_country_to_vectorise,
  nhl_shift_charts = nhl_shift_charts_TO_VECTORISE,
  nhl_glossary = nhl_glossary_TO_VECTORISE,
  nhl_games = nhl_games_TO_VECTORISE,
  nhl_seasons = nhl_seasons_TO_VECTORISE,
  nhl_franchises = nhl_franchises_TO_VECTORISE,
  nhl_draft = nhl_draft_TO_VECTORISE
)

# Apply function_vectorizer to each function in the list
vectorized_functions <- lapply(names(functions_to_vectorize), function(name) {
  function_vectorizer(
    func = functions_to_vectorize[[name]],
    arg_name = 'lang',
    all_possible = c('en', 'fr', 'ru')
  )
})


# Assign the vectorized functions to their respective names
names(vectorized_functions) <- names(functions_to_vectorize)
# Extract the individual vectorized functions
nhl_teams <- vectorized_functions$nhl_teams
nhl_country <- vectorized_functions$nhl_country
nhl_shift_charts <- vectorized_functions$nhl_shift_charts
nhl_glossary <- vectorized_functions$nhl_glossary
nhl_games <- vectorized_functions$nhl_games
nhl_seasons <- vectorized_functions$nhl_seasons
nhl_franchises <- vectorized_functions$nhl_franchises
nhl_draft <- vectorized_functions$nhl_draft
