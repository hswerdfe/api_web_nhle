




source(file.path('R', 'source_here.R'))

here_source('cache_vec.R')
here_source('season_team_vector.R')

require(glue)
require(purrr)
require(dplyr)



# #https://api-web.nhle.com/v1/gamecenter/2023020204/boxscore
# 
# #get_url_base(pattern = 'gamecenter/2023020204/boxscore', url_type = url_type)  
# #https://api-web.nhle.com/v1/standings/2023-11-10
# boxscore_BASE(game = 2023020204)




# Function to recursively flatten nested lists and concatenate names
flatten_list <- function(lst, parent_name = "",  sep = '_') {
  flat_list <- list()
  
  for (name in names(lst)) {
    item <- lst[[name]]
    
    # Construct the new name by appending the parent name
    new_name <- ifelse(parent_name == "", name, paste(parent_name, name, sep = sep))
    
    if (is.list(item) && !is.data.frame(item)) {
      # Recursively flatten the nested list
      flat_list <- c(flat_list, flatten_list(item, new_name))
    } else {
      # Append the item with its new name
      flat_list[[new_name]] <- item
    }
  }
  
  return(flat_list)
}


extract_dataframes <- function(dat,   base_name = ''){
  dat_2  <- 
    map2(names(dat), dat, \(.nm, .val){
    #.nm = 'playerByGameStats'
    #.val = dat[[.nm]]
    if (inherits(.val, 'data.frame')){
      if (nrow(.val) > 0){
        return(.val)
      }
    }
    imap
    if (inherits(.val, 'list')){
      ret_val = extract_dataframes(dat = .val)
      
      if (length(ret_val) == 0 ){
        return(NULL)
      }
      return(ret_val)
    }
    return (NULL)
  })|>
    set_names(paste0(base_name ,names(dat))) %>%
    Filter(Negate(is.null), .)  
  
  flatten_list(dat_2)
}

#' boxscore_all_BASE
#'
#' @param game  A game ID
#' @param pattern 
#' @param url_type 
#'
#' @return
#' @export
#'
#' @examples
#'     x = boxscore_all_BASE(game = 2023020204)
#'     x$tombstone$homeTeam_score
#'     x$tombstone$awayTeam_score
boxscore_all_BASE <- function(
    game,
    pattern = 'gamecenter/{game}/boxscore',
    url_type = 'base'
){
  dat <- get_url_base(pattern = pattern, url_type = url_type)  
  
  tombstone <- dat |> nhl_as_flat_tibble()
  db <- dat |> 
    extract_dataframes() |>
    map(~{
      .x |>
        mutate(gameID = game)
    })
  
  db_pos  <- 
    c('goalies', 'defense', 'forwards') %>%
    purrr::set_names() |>
    map(\(.pos){
      #.pos  = 'goalies'
      db_2 <- db[grepl(.pos, names(db))]
      
      curr_dat <- 
        db_2 |> 
        names() |>
        str_remove_all(.pos) |>
        str_remove_all('playerByGameStats') |>
        str_remove_all('_')  |>
        str_remove_all('Team') %>% 
        set_names(db_2, .) %>%
        map2_dfr(names(.), ., \(.nm, .x){
          .x |> 
            mutate(home_or_away  = .nm)
        }) 
      curr_dat
    })
  
  db_pos$skaters  <-
    bind_rows(
      db_pos$forwards,
      db_pos$defense
    )
  db_pos$forwards <- NULL
  db_pos$defense <- NULL
  
  c(
    list(
      tombstone = tombstone
    ),
    db_pos
  )
  
}




# 
# schedule <- cache_vect_maker(
#   func = schedule_BASE,
#   x_vec_nms  =  c('team', 'season'),
#   all_possible  = list(
#     team = teams(),
#     season = seasons()
#   ), 
#   is_valid_combo = is_valid_season
# )
