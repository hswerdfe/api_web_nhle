

source(file.path('R', 'source_here.R'))
here_source('download.R')
here_source('schedule.R')
here_source('players.R')
here_source('boxscore.R')
here_source('roster.R')
here_source('game.R')


all_teams_season_combo <- expand_vector_lst(
  args_vec = list(
    team = teams(),
    season = seasons()
  ), 
  is_valid_combo = is_valid_season
)


all_game_ids <- expand_vector_lst(
  args_vec = 
    list(
      game = games_all() 
    ), 
    is_valid_combo = \(...){TRUE}
)


############################
# Download all the Schedules
download_data(
  func = schedule_BASE,
  all_args = all_teams_season_combo |> sample_frac(size = 1) ,
  file_name_prefix = 'schedule_'
)



############################
# Download all the player base data
download_data(
  func = player_all_BASE,
  all_args = expand_vector_lst(
    args_vec = list(player = player_ids()), is_valid_combo = \(...){TRUE}) ,
  file_name_prefix = 'player_'
)



############################
# Download all the box-scores for every game
download_data(
  func = boxscore_all_BASE,
  all_args = all_game_ids |>  sample_frac(size = 1),
  file_name_prefix = 'boxscore_'
)




############################
#  Download ALL the data on the landing page of each game...
download_data(
  func = game_landing_BASE,
  all_args = all_game_ids |>  sample_frac(size = 1),
  file_name_prefix = 'game_landing_'
)



############################
# Download all  the roster data
download_data(
  func = roster_BASE,
  all_args = all_teams_season_combo |> sample_frac(size = 1) ,
  file_name_prefix = 'roster_'
)



############################
# Download the standings data
games_df <- read_feather_here('game_landing_tombstone.feather')
game_dates <- 
  games_df |>
  distinct(gameDate) |>
  rename(dt := gameDate) |>
  mutate(dt = as.Date(dt))

non_game_dates <- 
  tibble(dt = seq(from  = min(game_dates$dt), to = max(game_dates$dt), by = 1))  |>
  anti_join(game_dates)


standings_BASE(dt = games |> pull(gameDate) |> sample(1))
############################
# Download all the box-scores for every game
standings_db <- 
  download_data(
    func = standings_BASE,
    all_args = game_dates |> sample_frac(1),
    file_name_prefix = 'standings_'
  )




