
source(file.path('R', 'source_here.R'))

here_source('cache_vec.R')
here_source('roster.R')



#' player_ids
#' 
#'   get a vector of player IDs that are unique
#'
#' @param team default 'all' as three digit tricode like 'TOR', 'OTT', 'MTL'
#' @param season  default 'all' an 8 digit numeric season like 20232024
#' @param ... passed to roster  furnction
#'
#' @return a vector of player IDs
#' @export
#'
#' @examples
#'     player_ids()  
#'     player_ids(team = 'TOR', season = 19771978)
player_ids <- function(team = 'all',  season = 'all', ...){
  roster(team = team,  season = season, ...)  |> pull(id) |> unique() |> sort()
}




#' player_pluck_BASE
#'
#'    gets the data from a given players landing page
#'
#' @param player  a player ID
#' @param pluck   an element to pluck
#' @param pattern   default to 'player/{player}/landing'
#' @param url_type default to 'base'
#' @param get_func Defaults to get_url_possibly_cached
#'
#' @return  Data element from the players landing page data
#' @export
#'
#' @examples
player_pluck_BASE <- function(
    player, 
    pluck,
    pattern = 'player/{player}/landing', 
    url_type = 'base',   
    get_func = get_url_possibly_not_cached
){
  ret_val <- get_url_base(pattern = pattern, url_type = url_type, get_func = get_func)  
  
  if (is.null(ret_val)){
    return(tibble())
  }

  ret_val |>  
    nhl_pluck(pluck)   |>
    mutate(playerId = player)
}

#' player_tombstone_BASE, player_tombstone
#'
#' @param player 
#' @param pluck 
#' @param pattern 
#' @param url_type 
#' @param get_func 
#'
#' @return
#' @export
#'
#' @examples
#'      player_tombstone_BASE(player  = sample(p_ids, 1))
#'      player_tombstone_BASE(player  = 8445813)
#'      player_tombstone_BASE(player  = 8448119)
#'      player_tombstone(player  = sample(p_ids, 3))
#'      player_tombstone(player = c(8451874, 8477314, 8484198, 8470760, 8469492, 8477510))
#'      player_tombstone(player = c(8451874, 'NOT_A_PLAYER_NUMBER', 8484198))
player_tombstone_BASE <- function(
    player, 
    pattern = 'player/{player}/landing', 
    url_type = 'base',   
    get_func = get_url_possibly_not_cached
){
  get_url_base(pattern = pattern, url_type = url_type, get_func = get_func)  |> 
    nhl_as_flat_tibble()  |> 
    distinct()
  
}
player_tombstone <- cache_vect_maker(
  func = player_tombstone_BASE,
  x_vec_nms  =  c('player'),
  all_possible  = list(
    player = player_ids()
  ), 
  is_valid_combo = \(...){TRUE}
)




#' player_awards
#'
#' @param player   a  player ID normally starts with 84 and is 7 digits long like 8478402, can also be a vector of player ids or the word 'all'
#' @param pluck  default to 'Awards'
#' @param ...   passwed to player_pluck_BASE
#' @param unnest_col default 'seasons'
#'
#' @return a dataframe of player awards  and seasons
#' @export
#'
#' @examples
player_awards_BASE <- function(
  player,  
  pluck   = 'awards',
  ..., 
  unnest_col = 'seasons'
){
  ret_pluck <- player_pluck_BASE(player = player, pluck = pluck, ...) 
  if ( !  unnest_col  %in% colnames(ret_pluck) ){
    return(ret_pluck)
  }
  ret_pluck |>  
    unnest(!!sym(unnest_col))
}
player_awards <- cache_vect_maker(
  func = player_awards_BASE,
  x_vec_nms  =  c('player'),
  all_possible  = list(
    player = player_ids()
  ), 
  is_valid_combo = \(...){TRUE}
)



#' player_season_totals
#'
#' @param player    a  player ID normally starts with 84 and is 7 digits long like 8478402, can also be a vector of player ids or the word 'all'
#' @param pluck     Defaults to seasonTotals
#' @param ...       passed to player_pluck_BASE
#'
#' @return
#' @export
#'
#' @examples
player_season_totals_BASE  <- function(
    player  ,
    pluck   = 'seasonTotals',
    ...
  ){
  ret_pluck <- player_pluck_BASE(player = player, pluck = pluck, ...) 
  ret_pluck
}
player_season_totals <- cache_vect_maker(
  func = player_season_totals_BASE,
  x_vec_nms  =  c('player'),
  all_possible  = list(
    player = player_ids()
  ), 
  is_valid_combo = \(...){TRUE}
)
#player_tombstone(player = 'all')


#' Title
#'
#' @param player 
#' @param pattern 
#' @param url_type 
#' @param get_func 
#'
#' @return
#' @export
#'
#' @examples
#'   player_all_BASE(8482460)
#'   
player_all_BASE  <- function(
    player,
    pattern = 'player/{player}/landing', 
    url_type = 'base',   
    get_func = get_url_possibly_not_cached
){
  #player = 8478402 
  #player = 8460580 
  dat <- get_url_base(pattern = pattern, url_type = url_type, get_func = get_func)  

  awards  <- dat |> 
    nhl_pluck(nm   = 'awards')   |>
    mutate(playerId = player) 
    
  
  awards <- 
    if ( !  'seasons'  %in% colnames(awards) ){
      awards
    }else {
      awards |>  
        unnest('seasons')  
    }
  
  
  list(
    tombstone =  dat |> 
      nhl_as_flat_tibble()  |> 
      distinct(), 
    awards  = awards,
    seasonTotals  = dat |> 
      nhl_pluck(nm   = 'seasonTotals') 
  )
  
}



# 
# 
# download_xx <- function(
#   func  = player_all_BASE,
#   all_args = p_ids,
#   dir = file.path('data','download'),
#   base_name = 'player_{data_type}.feather' 
#   ){
#   
# 
#   db <- list()
#   # Iterate over all_args and update lst
#   for (i in seq_along(all_args)) {
#     arg <- all_args[i]
#     Sys.time()
#     cat(as.character(Sys.time()), i, arg,sep = ',  ' )
#     cat('\n ')
#     result <- tryCatch({
#       do.call(func, arg)
#       
#     }, error = function(e) { 
#       glue_message("Error: {e$message} on argument:, {arg}")
#       return(NULL)
#     })
#     
#     if (!is.null(result)) {
#       db <- map2(result, names(result), ~ bind_rows(db[[.y]], .x))
#     }
#     
#     # Save intermediate results every 5 iterations
#     if (i %% 50 == 0) {
#       save_db(db, dir  = dir, base_name = base_name)
#     }
#   }
#   
#   # Save final results
#   save_db(db, dir = dir, base_name = base_name)
# }
# 
# 





# download
# 
# 
# 
# p_ids <- player_ids()
# player_season_totals(player = sample(p_ids, 3))
# df <- player_season_totals(player  = 'all')
# 
# 
# 
# 
# sample(p_ids, 5)
# x  = sample(p_ids, 250)
# player_awards(player  = c(8478402, 8478402))
# player_awards(player  = x)
# 
# player_season_totals(player  = c(8478402, 8478402))
# df <- player_season_totals(player  = 'all')
# 
# 
# cache_vec_save(player_awards)
# cache_vec_save(player_season_totals)
# 
# player_season_totals_BASE(8478402)
# player_season_totals_BASE(8446668)
# seasonTotals
# 
# cache_vec_save(player_awards)
# cache_vec_save(player_season_totals)
# get_url_base(player = 8478402, url_type = 'base')
# player_awards_base(player = 8446668 )
# player_pluck_BASE(player = 8478402, pluck  = 'seasonTotals' )
# player_pluck_BASE(player = 8478402, pluck  = 'seasonTotals' )
# player_BASE
# 
# player_ids()
# pattern = 'player/{player}/landing'
# url_type = 'base'
# player = 8478402
# player  =  8478402
# ret_val <- get_url_base(pattern = pattern, url_type = url_type)  
# get_url_base
# glue::glue('player/{player}/landing') |>
#   nhl_get() |>
#   nhl_as_tibble()
